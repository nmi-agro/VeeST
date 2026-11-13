# extract_docx_review.R
#
# Haalt opmerkingen (comments) en bijgehouden wijzigingen (track changes) uit
# een Word-rapport en schrijft ze naar een Markdown-bestand dat Claude Code kan
# gebruiken om de wijzigingen in de bron-.qmd door te voeren.
#
# Gebruik vanaf de command line:
#   Rscript extract_docx_review.R rapport.docx [uitvoer.md]
#
# Of in R / Positron:
#   source("extract_docx_review.R")
#   extract_docx_review("rapport.docx")
#
# Afhankelijkheid: xml2

ns_w <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  w14 = "http://schemas.microsoft.com/office/word/2010/wordml",
  w15 = "http://schemas.microsoft.com/office/word/2012/wordml"
)

#' Lees een XML-onderdeel uit een docx
#'
#' @param docx_dir Map waarin de docx is uitgepakt.
#' @param part Relatief pad van het onderdeel, bijv. `"word/document.xml"`.
#'
#' @return Een `xml_document`, of `NULL` als het onderdeel niet bestaat.
#' @keywords internal
read_part <- function(docx_dir, part) {
  path <- file.path(docx_dir, part)
  if (!file.exists(path)) {
    return(NULL)
  }
  xml2::read_xml(path)
}

#' Klap witruimte samen en trim
#'
#' @param x Character vector.
#'
#' @return Character vector zonder dubbele spaties.
#' @keywords internal
squish <- function(x) {
  trimws(gsub("[[:space:]]+", " ", x))
}

#' Splits een tekstblok (alinea of tabelrij) in segmenten
#'
#' Elk segment is een stuk tekst met een status: ongewijzigd (`"equal"`),
#' ingevoegd (`"ins"`) of verwijderd (`"del"`). Markeringen van het begin en
#' einde van opmerkingen worden als aparte segmenten meegenomen.
#'
#' @param node Een `w:p` of `w:tr` node.
#'
#' @return Een `data.frame` met kolommen `kind`, `text`, `type`, `author`,
#'   `date` en `comment_id`.
#' @keywords internal
block_segments <- function(node) {
  xp <- paste(
    ".//w:t", ".//w:delText", ".//w:tab", ".//w:br", ".//w:noBreakHyphen",
    ".//w:commentRangeStart", ".//w:commentRangeEnd", ".//w:commentReference",
    "self::w:tr/w:tc[position() > 1]",
    sep = " | "
  )
  items <- xml2::xml_find_all(node, xp, ns_w)
  if (length(items) == 0) {
    return(data.frame())
  }

  rows <- lapply(items, function(it) {
    nm <- xml2::xml_name(it)
    if (nm %in% c("commentRangeStart", "commentRangeEnd", "commentReference")) {
      return(data.frame(
        kind = nm, text = "", type = "equal", author = NA_character_,
        date = NA_character_, comment_id = xml2::xml_attr(it, "w:id", ns_w)
      ))
    }
    text <- switch(nm,
      t = xml2::xml_text(it),
      delText = xml2::xml_text(it),
      tab = " ",
      br = " ",
      noBreakHyphen = "-",
      tc = " | ",
      ""
    )
    change <- xml2::xml_find_first(
      it, "ancestor::*[self::w:ins or self::w:del or self::w:moveFrom or self::w:moveTo][1]", ns_w
    )
    type <- "equal"
    author <- NA_character_
    date <- NA_character_
    if (!inherits(change, "xml_missing")) {
      type <- if (xml2::xml_name(change) %in% c("ins", "moveTo")) "ins" else "del"
      author <- xml2::xml_attr(change, "w:author", ns_w)
      date <- xml2::xml_attr(change, "w:date", ns_w)
    }
    if (nm == "delText") {
      type <- "del"
    }
    data.frame(
      kind = "text", text = text, type = type, author = author,
      date = date, comment_id = NA_character_
    )
  })
  do.call(rbind, rows)
}

#' Maak een tekstweergave van segmenten
#'
#' @param seg Segmenten uit [block_segments()].
#' @param version `"original"` (zonder invoegingen), `"revised"` (zonder
#'   verwijderingen) of `"markup"` (met `[-verwijderd-]` en `{+ingevoegd+}`).
#'
#' @return Een enkele string.
#' @keywords internal
segments_to_text <- function(seg, version = c("original", "revised", "markup")) {
  version <- match.arg(version)
  seg <- seg[seg$kind == "text", , drop = FALSE]
  if (nrow(seg) == 0) {
    return("")
  }
  if (version == "original") {
    return(squish(paste(seg$text[seg$type != "ins"], collapse = "")))
  }
  if (version == "revised") {
    return(squish(paste(seg$text[seg$type != "del"], collapse = "")))
  }
  # markup: aaneengesloten segmenten van hetzelfde type samenvoegen
  run_id <- cumsum(c(TRUE, seg$type[-1] != seg$type[-nrow(seg)]))
  parts <- vapply(split(seq_len(nrow(seg)), run_id), function(idx) {
    txt <- paste(seg$text[idx], collapse = "")
    switch(seg$type[idx[1]],
      ins = paste0("{+", txt, "+}"),
      del = paste0("[-", txt, "-]"),
      txt
    )
  }, character(1))
  squish(paste(parts, collapse = ""))
}

#' Haal opmerkingen en threads op
#'
#' @param comments_xml `xml_document` van `word/comments.xml`.
#' @param ext_xml `xml_document` van `word/commentsExtended.xml` (mag `NULL`).
#'
#' @return Een `data.frame` met per opmerking `id`, `author`, `date`, `text`,
#'   `para_id`, `parent_id` (id van de opmerking waarop gereageerd wordt) en
#'   `done` (opgelost in Word).
#' @keywords internal
read_comments <- function(comments_xml, ext_xml) {
  if (is.null(comments_xml)) {
    return(data.frame())
  }
  nodes <- xml2::xml_find_all(comments_xml, "//w:comment", ns_w)
  out <- data.frame(
    id = xml2::xml_attr(nodes, "w:id", ns_w),
    author = xml2::xml_attr(nodes, "w:author", ns_w),
    date = substr(xml2::xml_attr(nodes, "w:date", ns_w), 1, 10),
    text = vapply(nodes, function(n) {
      paras <- xml2::xml_find_all(n, ".//w:p", ns_w)
      txt <- vapply(paras, function(p) {
        squish(paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns_w)), collapse = ""))
      }, character(1))
      paste(txt[txt != ""], collapse = "\n")
    }, character(1)),
    para_id = vapply(nodes, function(n) {
      paras <- xml2::xml_find_all(n, ".//w:p", ns_w)
      if (length(paras) == 0) NA_character_ else xml2::xml_attr(paras[[length(paras)]], "w14:paraId", ns_w)
    }, character(1)),
    parent_id = NA_character_,
    done = FALSE
  )
  if (!is.null(ext_xml)) {
    ex <- xml2::xml_find_all(ext_xml, "//w15:commentEx", ns_w)
    ex_para <- xml2::xml_attr(ex, "w15:paraId", ns_w)
    ex_parent <- xml2::xml_attr(ex, "w15:paraIdParent", ns_w)
    ex_done <- xml2::xml_attr(ex, "w15:done", ns_w) == "1"
    m <- match(out$para_id, ex_para)
    out$done <- !is.na(m) & ex_done[m] %in% TRUE
    parent_para <- ex_parent[m]
    out$parent_id <- out$id[match(parent_para, out$para_id)]
  }
  out
}

#' Haal opmerkingen en bijgehouden wijzigingen uit een docx
#'
#' Leest een Word-document met opmerkingen en track changes en schrijft een
#' Markdown-overzicht met per wijziging en per opmerking de bijbehorende kop,
#' de oorspronkelijke en de herziene tekst. Dat overzicht is bedoeld als
#' invoer voor Claude Code om de wijzigingen in de bron-.qmd door te voeren.
#'
#' Tabellen worden per rij behandeld (cellen gescheiden door `|`), zodat
#' wijzigingen in tabellen herkenbaar blijven.
#'
#' @param docx Pad naar het .docx-bestand.
#' @param out_md Pad voor het Markdown-bestand. Standaard naast de docx met
#'   achtervoegsel `_review.md`.
#' @param include_resolved Logisch; ook in Word als opgelost gemarkeerde
#'   opmerkingen opnemen? Standaard `TRUE` (ze worden dan wel als opgelost
#'   gemarkeerd).
#'
#' @return Onzichtbaar een lijst met `changes` en `comments` (data.frames).
#'   Als neveneffect wordt `out_md` geschreven.
#' @export
#'
#' @examples
#' \dontrun{
#' extract_docx_review("rapport_modellering_VeeST_v1.docx")
#' }
extract_docx_review <- function(docx,
                                out_md = sub("\\.docx$", "_review.md", docx),
                                include_resolved = TRUE) {
  stopifnot(file.exists(docx), requireNamespace("xml2", quietly = TRUE))

  tmp <- tempfile("docx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(docx, exdir = tmp)

  doc <- read_part(tmp, "word/document.xml")
  comments <- read_comments(
    read_part(tmp, "word/comments.xml"),
    read_part(tmp, "word/commentsExtended.xml")
  )

  # Blokken: alinea's (ook in lay-outtabellen) en binnenste tabelrijen
  blocks <- xml2::xml_find_all(
    doc,
    paste(
      "/w:document/w:body//w:p[not(ancestor::w:txbxContent)][not(ancestor::w:tr) or ancestor::w:tr[1][.//w:tr]]",
      "/w:document/w:body//w:tr[not(.//w:tr)]",
      sep = " | "
    ),
    ns_w
  )

  heading_trail <- character(0)
  open_comments <- character(0)
  anchor <- list()
  context <- list()
  changes <- list()

  for (b in blocks) {
    seg <- block_segments(b)
    if (nrow(seg) == 0) {
      next
    }
    style <- xml2::xml_attr(xml2::xml_find_first(b, "./w:pPr/w:pStyle", ns_w), "w:val", ns_w)
    is_table <- xml2::xml_name(b) == "tr"

    # Koppen bijhouden (Heading1..9, Kop1..9, Title)
    lvl <- suppressWarnings(as.integer(sub("^(Heading|Kop)([0-9])$", "\\2", style %||% "")))
    if (!is.na(lvl)) {
      heading_text <- segments_to_text(seg, "revised")
      if (!nzchar(heading_text)) {
        heading_text <- paste0(segments_to_text(seg, "original"), " [kop verwijderd]")
      }
      heading_trail <- c(heading_trail[seq_len(min(length(heading_trail), lvl - 1))], heading_text)
    }
    section <- paste(heading_trail[nzchar(heading_trail)], collapse = " > ")
    caption <- ""
    if (is_table && !grepl("^(Figuur|Figure).?[0-9]", segments_to_text(seg, "original"))) {
      starts_tab <- "[starts-with(normalize-space(.), 'Tabel') or starts-with(normalize-space(.), 'Table')]"
      cap <- xml2::xml_find_first(
        b,
        paste0(
          "ancestor::w:tbl[last()]//w:p[not(ancestor::w:tr[1][not(.//w:tr)])]", starts_tab,
          " | ancestor::w:tbl[last()]/preceding-sibling::*[1][self::w:p]", starts_tab
        ),
        ns_w
      )
      if (!inherits(cap, "xml_missing")) {
        caption <- substr(squish(xml2::xml_text(cap)), 1, 120)
      }
    }
    is_figure <- is_table && grepl("^(Figuur|Figure).?[0-9]", segments_to_text(seg, "original"))
    location <- if (is_figure) "figuur met bijschrift" else if (is_table) paste0("tabelrij", if (nzchar(caption)) paste0(" (", caption, ")") else "") else if (!is.na(lvl)) paste0("kop (niveau ", lvl, ")") else "alinea"

    block_text <- segments_to_text(seg, "revised")
    if (!nzchar(gsub("[| ]", "", block_text))) {
      block_text <- paste0(segments_to_text(seg, "original"), " [verwijderd]")
    }
    block_ids <- character(0)

    # Opmerkingsbereiken volgen (kunnen over blokken heen lopen)
    for (i in seq_len(nrow(seg))) {
      k <- seg$kind[i]
      id <- seg$comment_id[i]
      if (k == "commentRangeStart") {
        open_comments <- union(open_comments, id)
        block_ids <- union(block_ids, id)
      } else if (k == "commentRangeEnd") {
        open_comments <- setdiff(open_comments, id)
      } else if (k == "commentReference") {
        block_ids <- union(block_ids, id)
      } else if (seg$type[i] != "del") {
        for (oc in open_comments) {
          anchor[[oc]] <- paste0(anchor[[oc]] %||% "", seg$text[i])
          block_ids <- union(block_ids, oc)
        }
      }
    }
    for (id in block_ids) {
      if (is.null(context[[id]])) {
        context[[id]] <- list(section = section, location = location, text = block_text)
      }
    }

    # Bijgehouden wijzigingen
    if (any(seg$type != "equal")) {
      ch <- seg[seg$kind == "text" & seg$type != "equal", , drop = FALSE]
      changes[[length(changes) + 1]] <- data.frame(
        section = section,
        location = location,
        authors = paste(unique(stats::na.omit(ch$author)), collapse = ", "),
        original = segments_to_text(seg, "original"),
        revised = segments_to_text(seg, "revised"),
        markup = segments_to_text(seg, "markup")
      )
    }
  }

  changes <- if (length(changes)) do.call(rbind, changes) else data.frame()
  # Alleen-witruimte wijzigingen weglaten en reeksen volledig verwijderde
  # blokken (bijv. een geschrapte paragraaf met tabel) samenvoegen
  if (nrow(changes)) {
    changes <- changes[changes$original != changes$revised, , drop = FALSE]
    changes <- collapse_deleted_runs(changes)
  }

  if (nrow(comments)) {
    comments$anchor <- vapply(comments$id, function(id) {
      a <- squish(anchor[[id]] %||% "")
      if (nchar(a) > 400) paste0(substr(a, 1, 400), " ...") else a
    }, character(1))
    ctx <- lapply(comments$id, function(id) context[[id]] %||% list(section = "", location = "", text = ""))
    comments$section <- vapply(ctx, `[[`, character(1), "section")
    comments$location <- vapply(ctx, `[[`, character(1), "location")
    comments$context <- vapply(ctx, `[[`, character(1), "text")
    if (!include_resolved) {
      comments <- comments[!comments$done, , drop = FALSE]
    }
  }

  write_review_md(changes, comments, docx, out_md)
  message(
    "Geschreven: ", out_md, " (", nrow(changes), " gewijzigde blokken, ",
    sum(is.na(comments$parent_id)), " opmerkingen)"
  )
  invisible(list(changes = changes, comments = comments))
}

#' Voeg opeenvolgende volledig verwijderde blokken samen
#'
#' Een geschrapte passage van meerdere alinea's, figuren en tabelrijen levert
#' anders tientallen losse wijzigingen op. Deze functie vat zo'n reeks samen
#' tot een wijziging met het begin en einde van de verwijderde passage.
#'
#' @param changes `data.frame` met wijzigingen (kolommen `section`,
#'   `location`, `authors`, `original`, `revised`, `markup`).
#'
#' @return `data.frame` met dezelfde kolommen.
#' @keywords internal
collapse_deleted_runs <- function(changes) {
  full_del <- gsub("[| ]", "", changes$revised) == "" & changes$original != ""
  run <- cumsum(c(TRUE, !(full_del[-1] & full_del[-length(full_del)])))
  out <- lapply(split(seq_len(nrow(changes)), run), function(idx) {
    if (length(idx) == 1) {
      return(changes[idx, ])
    }
    first <- changes$original[idx[1]]
    last <- changes$original[idx[length(idx)]]
    short <- function(x, n = 250) if (nchar(x) > n) paste0(substr(x, 1, n), " ...") else x
    data.frame(
      section = changes$section[idx[1]],
      location = sprintf("%d opeenvolgende blokken volledig verwijderd (alinea's/koppen/tabelrijen)", length(idx)),
      authors = paste(unique(unlist(strsplit(changes$authors[idx], ", "))), collapse = ", "),
      original = paste(changes$original[idx], collapse = "\n"),
      revised = "",
      markup = paste0(
        "[-VERWIJDER de hele passage, vanaf: \"", short(first),
        "\" t/m: \"", short(last), "\"-]"
      )
    )
  })
  do.call(rbind, out)
}

#' Schrijf het review-overzicht als Markdown
#'
#' @param changes `data.frame` met wijzigingen.
#' @param comments `data.frame` met opmerkingen.
#' @param docx Pad naar de bron-docx (voor de kop).
#' @param out_md Uitvoerpad.
#'
#' @return Onzichtbaar `out_md`.
#' @keywords internal
write_review_md <- function(changes, comments, docx, out_md) {
  quote_block <- function(x) paste0("> ", gsub("\n", "\n> ", x))
  lines <- c(
    paste0("# Review: ", basename(docx)),
    "",
    paste0("Gegenereerd op ", format(Sys.time(), "%Y-%m-%d %H:%M"), " met extract_docx_review.R."),
    "",
    "Dit bestand bevat de bijgehouden wijzigingen (W-nummers) en opmerkingen (O-nummers)",
    "uit het Word-document. De tekst komt uit de **gerenderde** docx; in de .qmd kan dezelfde",
    "passage inline R-code (`` `r ...` ``), kruisverwijzingen (`@fig-...`), citaties (`[@...]`)",
    "of andere opmaak bevatten. Zoek daarom op een kenmerkend stuk tekst, niet op de hele alinea.",
    "",
    "Notatie in *Markup*: `[-verwijderde tekst-]` en `{+ingevoegde tekst+}`; *Nieuw* is de tekst na acceptatie.",
    "Tabelrijen staan als cellen gescheiden door ` | `.",
    "",
    paste0("- Gewijzigde blokken: ", nrow(changes)),
    paste0("- Opmerkingen (excl. reacties): ", if (nrow(comments)) sum(is.na(comments$parent_id)) else 0),
    "",
    "## Bijgehouden wijzigingen",
    ""
  )

  if (nrow(changes)) {
    for (i in seq_len(nrow(changes))) {
      r <- changes[i, ]
      lines <- c(
        lines,
        sprintf("### W%d - %s", i, if (nzchar(r$section)) r$section else "(voor eerste kop)"),
        "",
        sprintf("- Type: %s", r$location),
        sprintf("- Door: %s", r$authors),
        "",
        "**Markup:**", "", quote_block(r$markup), "",
        "**Nieuw:**", "", quote_block(r$revised), ""
      )
    }
  } else {
    lines <- c(lines, "_Geen bijgehouden wijzigingen gevonden._", "")
  }

  lines <- c(lines, "## Opmerkingen", "")
  roots <- if (nrow(comments)) comments[is.na(comments$parent_id), , drop = FALSE] else comments
  if (nrow(roots)) {
    for (i in seq_len(nrow(roots))) {
      r <- roots[i, ]
      replies <- comments[comments$parent_id %in% r$id, , drop = FALSE]
      lines <- c(
        lines,
        sprintf("### O%d - %s%s", i, if (nzchar(r$section)) r$section else "(voor eerste kop)",
          if (r$done) " [OPGELOST in Word]" else ""),
        "",
        sprintf("- Type: %s", r$location),
        sprintf("- Word-id: %s", r$id),
        "",
        "**Gemarkeerde tekst:**", "",
        quote_block(if (nzchar(r$anchor)) r$anchor else "(geen selectie; opmerking op een punt)"), "",
        "**Context (volledige alinea/rij):**", "", quote_block(r$context), "",
        "**Opmerking:**", "",
        sprintf("- **%s** (%s): %s", r$author, r$date, gsub("\n", " / ", r$text))
      )
      for (j in seq_len(nrow(replies))) {
        lines <- c(lines, sprintf(
          "  - Reactie **%s** (%s): %s",
          replies$author[j], replies$date[j], gsub("\n", " / ", replies$text[j])
        ))
      }
      lines <- c(lines, "")
    }
  } else {
    lines <- c(lines, "_Geen opmerkingen gevonden._", "")
  }

  writeLines(enc2utf8(lines), out_md, useBytes = TRUE)
  invisible(out_md)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Gebruik: Rscript extract_docx_review.R rapport.docx [uitvoer.md]")
  }
  if (length(args) >= 2) {
    extract_docx_review(args[1], args[2])
  } else {
    extract_docx_review(args[1])
  }
}
