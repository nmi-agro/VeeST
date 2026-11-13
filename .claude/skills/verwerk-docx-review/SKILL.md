---
name: verwerk-docx-review
description: Verwerk opmerkingen en bijgehouden wijzigingen uit een gereviewde Word-versie (.docx) van een rapport terug in de Quarto-bron (.qmd). Gebruik dit als iemand een docx met commentaar/track changes heeft teruggekregen en de wijzigingen in de qmd wil doorvoeren.
argument-hint: <pad/naar/gereviewd_rapport.docx> [pad/naar/rapport.qmd]
---

# Review uit Word verwerken in de .qmd

Het rapport wordt gerenderd vanuit een `.qmd`. Reviewers werken in de gerenderde
`.docx`. Deze skill haalt hun opmerkingen en track changes uit de docx en voert
ze door in de bron, zonder de docx zelf als bron te gebruiken.

Argumenten: `$ARGUMENTS` (eerste = docx; optioneel tweede = qmd).

## Stap 1 - Extraheren

Voer uit (vanuit de projectmap):

```bash
Rscript .claude/skills/verwerk-docx-review/scripts/extract_docx_review.R "<docx>"
```

Dit schrijft `<docx-naam>_review.md` naast de docx (vereist R-pakket `xml2`).
Lees dat bestand volledig. Het bevat:

- **W-items**: bijgehouden wijzigingen per alinea/tabelrij, met kop-pad,
  `[-verwijderd-]{+ingevoegd+}`-markup en de nieuwe tekst.
- **O-items**: opmerkingen met gemarkeerde tekst, context, auteur en reacties.

## Stap 2 - Bron vinden

- Neem de opgegeven qmd, anders de qmd met dezelfde basisnaam als de docx
  (zonder `_v1`, `_DvR` e.d.). Volg `{{< include >}}` en child-documenten.
- De docx is **gerenderd**: tekst in de qmd kan verschillen door
  inline R (`` `r ...` ``), code-opmaak (`` `var` ``), `**vet**`, citaties
  `[@key]`, kruisverwijzingen (`@fig-x` wordt "Figuur 10", `@tbl-x` wordt
  "Tabel 3") en typografische aanhalingstekens. Zoek op een kenmerkend
  stuk van 5-10 woorden, niet op de hele alinea.
- De qmd kan na het versturen van de docx al gewijzigd zijn. Kun je een
  passage niet vinden of is de wijziging al doorgevoerd: markeer als
  `niet gevonden` / `al verwerkt`, niet gokken.

## Stap 3 - Classificeren (eerst plan, nog niet bewerken)

Bepaal per item waar de tekst vandaan komt en wat er moet gebeuren:

| Herkomst in qmd | Aanpak |
|---|---|
| Lopende tekst / kop | Wijziging direct in de qmd-tekst |
| `#| fig-cap:` / `#| tbl-cap:` | Pas de chunk-optie aan |
| Tabelinhoud, as-labels, legenda (uit R) | Pas de R-code of lookup aan (bijv. labelvector, `parameters_*.csv`); nooit de gerenderde waarden hardcoden |
| Getal uit inline R | Niet overschrijven met een vast getal; als reviewer een ander getal wil, is dat een inhoudelijk punt |

Bijzondere gevallen:

- **Ingevoegde tekst die een vraag of notitie is** (bijv. "Misschien ... ?",
  "Note to self") behandel je als opmerking, niet invoegen.
- **Grote verwijderingen** ("N opeenvolgende blokken volledig verwijderd"):
  controleer of dezelfde tekst elders als invoeging of ongewijzigd voorkomt
  (= verplaatsing). Verplaats dan inclusief bijbehorende code-chunks en
  labels; controleer of `@fig-`/`@tbl-`verwijzingen elders nog kloppen.
- **Opmerkingen**:
  - tekstueel/redactioneel verzoek (formulering, eenheid, verduidelijking)
    → voorstel voor tekstwijziging;
  - inhoudelijke vraag, nieuwe analyse, ander model, extra hoofdstuk
    → **niet** zelf uitvoeren; opnemen als open punt met een korte
    conceptreactie of suggestie;
  - "Note to self" van de auteur → open punt.
- Opmerkingen gemarkeerd als `[OPGELOST in Word]` alleen noemen.

Toon het plan als tabel (ID, locatie in qmd `bestand:regel`, actie,
categorie: `direct` / `voorstel` / `open punt` / `niet gevonden`) en vraag
de gebruiker om akkoord voordat je bewerkt.

## Stap 4 - Doorvoeren

- Voer alleen de goedgekeurde items door, met minimale bewerkingen.
- Behoud qmd-syntax: chunk-opties, labels, kruisverwijzingen, citaties,
  inline R. Geen spelling of stijl buiten de items aanpassen.
- Wijzig geen data, modelinstellingen of analysecode zonder expliciete
  opdracht (labels/opmaak in code mag als het item daarom vraagt).
- Render niet tenzij gevraagd. Eventueel snelle syntaxcheck:
  `quarto inspect <qmd>`.

## Stap 5 - Verslag

Schrijf `<docx-naam>_verwerking.md` naast de docx met per W/O-item:
status (`verwerkt`, `voorstel`, `open punt`, `niet gevonden`, `al verwerkt`),
`bestand:regel` en een korte toelichting. Zet de open punten (inhoudelijke
vragen voor de auteur, met conceptreactie) apart onderaan, zodat die in Word
beantwoord kunnen worden.

Sluit af met de opmerking dat de wijzigingen door een mens gecontroleerd
moeten worden (bijv. via `git diff`) voordat het rapport opnieuw wordt
verstuurd.
