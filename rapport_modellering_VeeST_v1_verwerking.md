# Verwerking review: rapport_modellering_VeeST_v1.docx

Verwerkt op 2026-10-20 in `rapport_modellering_VeeST.qmd`, op basis van `rapport_modellering_VeeST_v1_review.md`.
Reviewers: Debby van Rotterdam, FLORON.

**Let op:** de docx is gemaakt van een eerdere versie van de qmd. Sindsdien is onder meer de interpretatie van de VIP-figuur herschreven (modelrun 15-10-2026). Wijzigingen in die passages zijn daarom naar de nieuwe tekst vertaald in plaats van letterlijk overgenomen.

## Bijgehouden wijzigingen

| ID | Status | Locatie | Toelichting |
|---|---|---|---|
| W1 | verwerkt | rapport_modellering_VeeST.qmd:124 | "(eenmalige inventarisaties van WP1 en per sloot één traject tijdens de nulmeting van WP2)". |
| W2 | verwerkt | rapport_modellering_VeeST.qmd:130 | Zin "De dataset bevat locatiemetingen van meerdere waterschappen." verwijderd. |
| W3 | verwerkt | rapport_modellering_VeeST.qmd:132 | "Hydrologische variabelen" → "Slootvariabelen". |
| W4 | verwerkt (aangepast) | rapport_modellering_VeeST.qmd:239 | Diepte toegevoegd, maar als "(perceel op 50–80 cm en oever op 0–40 cm onder maaiveld)": draagkracht oever is volgens de methoden 0–40 cm, niet 50–80 cm. |
| W5/W6 | verwerkt | rapport_modellering_VeeST.qmd:420 en :430 | Zin "XGBoost en Random Forest hebben doorgaans meer data nodig…" verplaatst van de GAM-sectie naar het einde van de sectie Random Forest. |
| W7 | verwerkt | rapport_modellering_VeeST.qmd:465 | "onderscheidbaar zijn" → "te onderscheiden zijn". |
| W8 | verwerkt | rapport_modellering_VeeST.qmd:474 | "tussen de gebruikte databestanden in de GIS-clustering en veld". |
| W9 | verwerkt (aangepast) | rapport_modellering_VeeST.qmd:614 | Op aanwijzing van de auteur geherformuleerd: indexering leidt altijd tot een nieuw vigerend peilbesluit en is dus geen verklaring. Toegevoegd: "Peilen worden in de praktijk ook lager gehouden omdat er anders water op maaiveld komt, bijvoorbeeld bij lagere percelen binnen een peilgebied of door bodemdaling." Het punt over de legger staat bij de open punten. |
| W10 | verwerkt | rapport_modellering_VeeST.qmd:657 | "ook" verwijderd. |
| W11 | verwerkt | rapport_modellering_VeeST.qmd:686 | "…meest soortenrijke sloten met relatief voedselarm slib…". |
| W12 | verwerkt | rapport_modellering_VeeST.qmd:687 | "een hoog gehalte P-totaal", "zeer hoge concentratie chloride". |
| W13 | open punt | rapport_modellering_VeeST.qmd:861 (fig-cap) | Ingevoegde vraag in het bijschrift, niet ingevoegd. Zie open punten. |
| W14 | verwerkt (vertaald) | rapport_modellering_VeeST.qmd:1002 | Tekst is herschreven. Stijl van de reviewer toegepast: de toelichting tussen haakjes is een eigen zin geworden. |
| W15 | verwerkt (vertaald) | rapport_modellering_VeeST.qmd:1004 | Tekst is herschreven; het deel over koebelasting is vervallen (koebelasting is in de huidige run geen voorspeller van slibdikte). De twee verklaringen van de reviewer voor het verband P-AL–slibdikte zijn toegevoegd. |
| W16 | verwerkt (aangepast) | rapport_modellering_VeeST.qmd:1006 | Haakjes met pijl omgezet in een zin, met "gaat samen met" in plaats van "leidt tot", omdat het model een samenhang toont en geen oorzaak-gevolg. |
| W17 | verwerkt | rapport_modellering_VeeST.qmd:1008 | "lichtdoorlating, instabielere waterbodem en/of een chemische samenstelling die beperkend is (P, S, NH₄⁺)". Het deel over ammonium is in de huidige tekst herschreven en vervalt. |
| W18 | verwerkt | rapport_modellering_VeeST.qmd:1266 (code tbl-xgb-diag-summary) | Eenheid verwijderd uit de cellen van RMSE en bias (staat al bij de doelvariabele); de ongebruikte merge met `rmse_unit` is weggehaald. |
| W19 | verwerkt (verplaatsing) | rapport_modellering_VeeST.qmd:1020 | De v1-docx bevatte de ALE-sectie twee keer; de reviewer verwijderde de tweede. De sectie staat nu direct na "Voorspellers die het meest bijdragen" (vóór "Validatie"). `f1()` en `opsomming()` zijn meeverhuisd naar de ALE-hulpchunk (:1027), omdat de ALE-sectie nu eerder wordt uitgevoerd. "hierboven bij de ALE-kantelpunten" → "hieronder" (:1012). |

## Opmerkingen

| ID | Status | Locatie | Toelichting |
|---|---|---|---|
| O1 | open punt | rapport_modellering_VeeST.qmd:141 (tbl-targets) | Zie open punten. |
| O2 | verwerkt | rapport_modellering_VeeST.qmd:178 | De methodetekst klopte niet: volgens de code (scripts/data_import_ppr.R:978-979) is de drooglegging berekend ten opzichte van het **gemeten** waterpeil, niet het NHI-peil. Tekst gecorrigeerd en een kanttekening toegevoegd dat het een momentopname is zonder informatie over zomer- en winterpeil. |
| O3 | al verwerkt | rapport_modellering_VeeST.qmd:252 | Eenheid van de koebelasting is in de huidige tekst uitgewerkt ("koeien per 100 m oever"). |
| O4 | verwerkt (aangepast) | rapport_modellering_VeeST.qmd:260 | Niet omgezet naar slibdikte/waterdiepte, omdat dat de beschreven berekening verandert (de inverse van een genormaliseerde verhouding is niet hetzelfde als een genormaliseerde omgekeerde verhouding). Wel een verduidelijkende zin toegevoegd. |
| O5 | open punt | rapport_modellering_VeeST.qmd:352 | Zie open punten. |
| O6/O7 | verwerkt | rapport_modellering_VeeST.qmd:474 | Toegevoegd dat een typering op kaartdata wezenlijk afwijkt van een typering op veldmetingen, met drooglegging als voorbeeld, en dat een veldbezoek onmisbaar is. |
| O8 | verwerkt | rapport_modellering_VeeST.qmd:517 | Toegevoegd dat BodemSchat als ruimtelijk model extreme waarden afvlakt. |
| O9 | verwerkt | rapport_modellering_VeeST.qmd:529 | Uitleg herschreven in stappen (drooglegging = maaiveld − peil; 0,34 − 0,20 = 0,14 m). |
| O10 | verwerkt | rapport_modellering_VeeST.qmd:712 (code tbl-performance) | R² zonder decimalen, RMSE in 3 significante cijfers, eenheid uit de cellen (consistent met W18). |
| O11 | verwerkt + open punt | rapport_modellering_VeeST.qmd:791 (code tbl-xgb-vs-rf-gap) | R² en verschil zonder decimalen. Vraag "waarom mist P-AL?" staat bij de open punten. |
| O12 | verwerkt | rapport_modellering_VeeST.qmd:858 | Uitleg toegevoegd waarom Gain variabelen met veel unieke waarden bevoordeelt. |
| O13 | verwerkt | rapport_modellering_VeeST.qmd:1004 | Stroming als alternatieve verklaring toegevoegd bij het verband taludhoek–slibdikte. |
| O14/O15 | open punt | rapport_modellering_VeeST.qmd:1080 (fig-xgb-kantelpunten) | Zie open punten. |
| O16 | open punt | rapport_modellering_VeeST.qmd:1600 | Zie open punten. |

## Open punten (voor beantwoording in Word)

**W9 (legger, Debby):** "Dit kan ook effect hebben op de legger t.o.v. de daadwerkelijke ruimte in een sloot om te baggeren."
*Conceptreactie:* Goed punt, maar valt buiten de scope van deze analyse. We nemen het mee als aandachtspunt voor de discussie over baggerbeheer in WP2/3.

**W13 (Debby):** "Misschien het model voor bedekking ondergedoken waterplanten verwijderen omdat dit zo'n zwak model is?"
*Conceptreactie:* Het model voor aantal oeversoorten is om die reden al uit de VIP-figuur gehaald. Voor bedekking ondergedoken planten is dit nog een keuze. [Auteur: R² en ruimtelijke CV van dit model in de huidige run controleren en dan besluiten.]

**O1 (Debby):** "Verschillende variabelen zijn in meerdere zones gemeten. Moet dit worden gespecificeerd? Draagkracht diepte?"
*Conceptreactie:* Ja, goed punt. We voegen de zone en diepte toe aan de naam van de doelvariabele, bijvoorbeeld "Draagkracht oever (0–40 cm)". Dit vraagt een aanpassing van `rf_target_names_dutch` in scripts/Analyses_dev_db.R (rond regel 4307) en een nieuwe run.

**O5 (Debby):** "Voor alle analyses of alleen voor de analyses die gevoelig zijn voor outliers?"
*Conceptreactie:* Alleen voor de clusteranalyse (k-means, gebaseerd op afstanden) en de as van de boxplot. Boommodellen (XGBoost, Random Forest) zijn ongevoelig voor zo'n monotone transformatie. [Auteur: nagaan of de GAM's de getransformeerde of ruwe waarden gebruiken en de zin daarop aanvullen.]

**O11 (Debby):** "Waarom mist P-AL hier?"
*Conceptreactie:* De tabel toont alleen doelvariabelen waarbij XGBoost meer dan 15 %-punt beter scoort dan Random Forest. Bij P-AL slib is het verschil kleiner; beide modellen presteren daar vergelijkbaar.

**O14/O15 (Debby):** "De kleur is vermoedelijk representatief voor de doelvariabele. Welke kleur is welke doelvariabele?"
*Conceptreactie:* Klopt, elke rij heeft een eigen kleur per doelvariabele. We voegen de naam van de doelvariabele toe aan elke rij of een legenda. [Auteur: `xgb_kantelpunt_plot` in scripts/Analyses_dev_db.R aanpassen.]

**O16 (FLORON):** "Ook een vergelijkbaar hoofdstuk maken voor risico op BKG-emissies."
*Conceptreactie:* Dit vraagt een nieuwe index en analyse en past mogelijk beter in een vervolgrapportage. [Auteur: besluiten of dit in deze versie komt.]

## Controle

- Het rapport is niet gerenderd. `quarto inspect rapport_modellering_VeeST.qmd` slaagt.
- Alle wijzigingen moeten door een mens worden gecontroleerd, bijvoorbeeld met `git diff rapport_modellering_VeeST.qmd`, voordat het rapport opnieuw wordt verstuurd. Let in het bijzonder op de verplaatsing van de ALE-sectie (W19): render het rapport om te controleren dat alle inline-getallen en verwijzingen nog kloppen.
