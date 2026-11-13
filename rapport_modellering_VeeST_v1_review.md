# Review: rapport_modellering_VeeST_v1.docx

Gegenereerd op 2026-10-20 11:53 met extract_docx_review.R.

Dit bestand bevat de bijgehouden wijzigingen (W-nummers) en opmerkingen (O-nummers)
uit het Word-document. De tekst komt uit de **gerenderde** docx; in de .qmd kan dezelfde
passage inline R-code (`` `r ...` ``), kruisverwijzingen (`@fig-...`), citaties (`[@...]`)
of andere opmaak bevatten. Zoek daarom op een kenmerkend stuk tekst, niet op de hele alinea.

Notatie in *Markup*: `[-verwijderde tekst-]` en `{+ingevoegde tekst+}`; *Nieuw* is de tekst na acceptatie.
Tabelrijen staan als cellen gescheiden door ` | `.

- Gewijzigde blokken: 19
- Opmerkingen (excl. reacties): 16

## Bijgehouden wijzigingen

### W1 - Inleiding

- Type: alinea
- Door: FLORON

**Markup:**

> De analyses zijn uitgevoerd op een gecombineerde dataset van veldmetingen ({+eenmalige inventarisaties van +}WP1 en{+ per sloot één traject tijdens de nul-meting van+} WP2), gefilterd op monsters (meettraject en meetmoment) met complete abiotische én vegetatieopnamen.

**Nieuw:**

> De analyses zijn uitgevoerd op een gecombineerde dataset van veldmetingen (eenmalige inventarisaties van WP1 en per sloot één traject tijdens de nul-meting van WP2), gefilterd op monsters (meettraject en meetmoment) met complete abiotische én vegetatieopnamen.

### W2 - Methoden > Data en voorspellende variabelen

- Type: alinea
- Door: FLORON

**Markup:**

> [-De dataset bevat locatiemetingen van meerdere waterschappen. -]Voor de XGBoost- en Random Forest-modellen (hieronder) is de data eerst gefilterd op MeenemenDataAnalyse_totaal == 'ja': een vlag die aangeeft welke meting per sloot representatief is voor de reguliere situatie. Deze filter is nodig omdat WP2-sloten in sommige gevallen in meerdere jaren en/of onder meerdere experimentele behandelingen zijn bemeten; zonder filtering zou zo’n sloot meerdere keren in de dataset voorkomen en zwaarder meewegen dan sloten met één meting, en zouden proefbehandelingen (die niet representatief zijn voor reguliere bedrijfsvoering) meegenomen worden als reguliere waarnemingen. Na deze filtering en de aanvullende complete.cases()-filtering per doelvariabele (verwijdering van rijen met ontbrekende waarden in doelvariabele of voorspellende variabelen) blijven gemiddeld ~190 sloten per doelvariabele over; een klein aantal sloten (10) komt ook na filtering nog met twee meetjaren voor, omdat er voor die sloten meerdere als “ja” gemarkeerde metingen bestaan. Na deze kwaliteitsfiltering op volledigheid en unieke trajecten per sloot (slechts één behandeling per sloot is meegenomen, waarbij de proefbehandelingen zijn weggelaten) zijn 33 voorspellende variabelen geselecteerd, verdeeld over:

**Nieuw:**

> Voor de XGBoost- en Random Forest-modellen (hieronder) is de data eerst gefilterd op MeenemenDataAnalyse_totaal == 'ja': een vlag die aangeeft welke meting per sloot representatief is voor de reguliere situatie. Deze filter is nodig omdat WP2-sloten in sommige gevallen in meerdere jaren en/of onder meerdere experimentele behandelingen zijn bemeten; zonder filtering zou zo’n sloot meerdere keren in de dataset voorkomen en zwaarder meewegen dan sloten met één meting, en zouden proefbehandelingen (die niet representatief zijn voor reguliere bedrijfsvoering) meegenomen worden als reguliere waarnemingen. Na deze filtering en de aanvullende complete.cases()-filtering per doelvariabele (verwijdering van rijen met ontbrekende waarden in doelvariabele of voorspellende variabelen) blijven gemiddeld ~190 sloten per doelvariabele over; een klein aantal sloten (10) komt ook na filtering nog met twee meetjaren voor, omdat er voor die sloten meerdere als “ja” gemarkeerde metingen bestaan. Na deze kwaliteitsfiltering op volledigheid en unieke trajecten per sloot (slechts één behandeling per sloot is meegenomen, waarbij de proefbehandelingen zijn weggelaten) zijn 33 voorspellende variabelen geselecteerd, verdeeld over:

### W3 - Methoden > Data en voorspellende variabelen

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> [-Hydrologische -]{+Sloot+}variabelen: drooglegging, maximale waterdiepte, doorzicht, waterbreedte

**Nieuw:**

> Slootvariabelen: drooglegging, maximale waterdiepte, doorzicht, waterbreedte

### W4 - Methoden > Data en voorspellende variabelen > Toegepaste datatransformaties of standaardisaties > Diagnostiek: spreiding draagkracht in de tijd versus tussen gebieden

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Voor beide draagkrachtmaten {+op een diepte van 50 – 80cm-mv +}is de spreiding binnen een locatie over de tijd van vergelijkbare orde van grootte als de spreiding tussen gebieden. Bij draagkracht perceel is de within-locatie spreiding zelfs licht groter dan de tussen-gebied spreiding; bij draagkracht oever is de tussen-gebied spreiding iets groter dan de within-locatie spreiding, maar het verschil is klein. Dit betekent dat een deel van de verschillen die in de gebieds-boxplots zichtbaar zijn, niet uitsluitend structurele verschillen tussen gebieden weerspiegelt, maar mede door jaar-op-jaar-variatie op dezelfde locatie kan worden verklaard (bijvoorbeeld door seizoensinvloeden, meetvariatie of lokale veranderingen).

**Nieuw:**

> Voor beide draagkrachtmaten op een diepte van 50 – 80cm-mv is de spreiding binnen een locatie over de tijd van vergelijkbare orde van grootte als de spreiding tussen gebieden. Bij draagkracht perceel is de within-locatie spreiding zelfs licht groter dan de tussen-gebied spreiding; bij draagkracht oever is de tussen-gebied spreiding iets groter dan de within-locatie spreiding, maar het verschil is klein. Dit betekent dat een deel van de verschillen die in de gebieds-boxplots zichtbaar zijn, niet uitsluitend structurele verschillen tussen gebieden weerspiegelt, maar mede door jaar-op-jaar-variatie op dezelfde locatie kan worden verklaard (bijvoorbeeld door seizoensinvloeden, meetvariatie of lokale veranderingen).

### W5 - Methoden > Random Forest

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> {+XGBoost en Random Forest hebben doorgaans meer data nodig om stabiele, generaliseerbare splitsingen te leren (een voorspellend model), met name wanneer veel voorspellende variabelen onderling gecorreleerd zijn.+}

**Nieuw:**

> XGBoost en Random Forest hebben doorgaans meer data nodig om stabiele, generaliseerbare splitsingen te leren (een voorspellend model), met name wanneer veel voorspellende variabelen onderling gecorreleerd zijn.

### W6 - Methoden > GAM met ruimtelijke smoothing

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> [-XGBoost en Random Forest hebben doorgaans meer data nodig om stabiele, generaliseerbare splitsingen te leren (een voorspellend model), met name wanneer veel voorspellende variabelen onderling gecorreleerd zijn.-]

**Nieuw:**

> 

### W7 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> De overeenkomst tussen ruimtelijk en variabele-gebaseerde clusterindeling geeft aan in hoeverre de GIS-clustertypen ook in de gemeten data {+te +}[-onderscheidbaar -]{+onderscheiden +}zijn. Een lage overeenkomst wijst op systematische afwijkingen tussen GIS-laag en veldmeting (zoals de OS- en droogleggingsverschillen die hieronder worden besproken), of op grote variatie binnen clusters.

**Nieuw:**

> De overeenkomst tussen ruimtelijk en variabele-gebaseerde clusterindeling geeft aan in hoeverre de GIS-clustertypen ook in de gemeten data te onderscheiden zijn. Een lage overeenkomst wijst op systematische afwijkingen tussen GIS-laag en veldmeting (zoals de OS- en droogleggingsverschillen die hieronder worden besproken), of op grote variatie binnen clusters.

### W8 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Dit betekent niet dat de clusterindeling uit de verkenningsfase onjuist is, maar dat de GIS-laag en de veldmeting verschillende schaalniveaus beschrijven en zijn gebaseerd op verschillende bronnen, waarbij de relatieve waarde van variabelen vergelijkbaar is maar de absolute waarde niet. De GIS-laag representeert een gemiddeld perceel binnen een gemiddeld peilvak in het Nederlandse veenweidegebied, terwijl de VeeST-metingen oevers en sloten op standplaatsniveau in beeld heeft gebracht. De clusterindeling blijft zinvol als ruimtelijk kader, maar de absolute drempelwaarden voor OS, klei en drooglegging zijn niet direct vergelijkbaar tussen {+de gebruikte databestanden in de +}GIS{+-clustering+} en veld.

**Nieuw:**

> Dit betekent niet dat de clusterindeling uit de verkenningsfase onjuist is, maar dat de GIS-laag en de veldmeting verschillende schaalniveaus beschrijven en zijn gebaseerd op verschillende bronnen, waarbij de relatieve waarde van variabelen vergelijkbaar is maar de absolute waarde niet. De GIS-laag representeert een gemiddeld perceel binnen een gemiddeld peilvak in het Nederlandse veenweidegebied, terwijl de VeeST-metingen oevers en sloten op standplaatsniveau in beeld heeft gebracht. De clusterindeling blijft zinvol als ruimtelijk kader, maar de absolute drempelwaarden voor OS, klei en drooglegging zijn niet direct vergelijkbaar tussen de gebruikte databestanden in de GIS-clustering en veld.

### W9 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters > Oppervlaktewaterpeilen VeeST meting tov praktijkpeilen in GIS laag (NOBV)

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> {+Prakrijkpeilen kunnen ook lager zijn door de indexering als gevolg van bodemdaling. Dit kan ook effect hebben op de legger tov de daadwerkelijke ruimte in een sloot om te baggeren (totaal ander punt, maar wilde het even uit mijn systeem hebben 😊)+}

**Nieuw:**

> Prakrijkpeilen kunnen ook lager zijn door de indexering als gevolg van bodemdaling. Dit kan ook effect hebben op de legger tov de daadwerkelijke ruimte in een sloot om te baggeren (totaal ander punt, maar wilde het even uit mijn systeem hebben 😊)

### W10 - Resultaten > Clusteranalyse abiotische variabelen

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Trofie veen is lager in cluster 3, 4, 6 en 8 (laagste trofiegehalte in veen) dan in de overige clusters. Cluster 3, 4 en 6 hebben ook de hoogste aantallen soorten waterplanten (zie Figuur 6), P-AL in slib is het laagst en Fe-P is het hoogst in deze clusters. Totaal-P in de oever is wel lager in cluster 3 en 4, maar juist niet in cluster 6. De relatie tussen de hoeveelheid P in de oever en de hoeveelheid P in het slib is [-ook -]zeer zwak (Figuur 1) en dus niet eenduidig.

**Nieuw:**

> Trofie veen is lager in cluster 3, 4, 6 en 8 (laagste trofiegehalte in veen) dan in de overige clusters. Cluster 3, 4 en 6 hebben ook de hoogste aantallen soorten waterplanten (zie Figuur 6), P-AL in slib is het laagst en Fe-P is het hoogst in deze clusters. Totaal-P in de oever is wel lager in cluster 3 en 4, maar juist niet in cluster 6. De relatie tussen de hoeveelheid P in de oever en de hoeveelheid P in het slib is zeer zwak (Figuur 1) en dus niet eenduidig.

### W11 - Resultaten > Clusteranalyse abiotische variabelen

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Cluster 4 springt er sterk uit door een zeer hoog opgelost ijzergehalte in de oever gecombineerd met een lage pH (3.76; een indicatie van zure, ijzerrijke omstandigheden, verzuurde/geoxideerde veenbodem), terwijl dit tegelijk een van de clusters is met de meest soortenrijke sloten [-is en -]{+met +}relatief voedselarm slib [-heeft -](lage P-AL in slib).

**Nieuw:**

> Cluster 4 springt er sterk uit door een zeer hoog opgelost ijzergehalte in de oever gecombineerd met een lage pH (3.76; een indicatie van zure, ijzerrijke omstandigheden, verzuurde/geoxideerde veenbodem), terwijl dit tegelijk een van de clusters is met de meest soortenrijke sloten met relatief voedselarm slib (lage P-AL in slib).

### W12 - Resultaten > Clusteranalyse abiotische variabelen

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Cluster 5 zijn enkele locaties in Assendelft met een zeer hoge P-AL in het slib, [-veel -]{+een hoog gehalte +}P-totaal in de oever, zeer [-veel -]{+hoge concentratie +}chloride in het poriewater (mediane concentratie 34 mg/l), lagere opgelost ijzergehalte (calciumchloride) en een veel hogere pH (calciumchloride) in de toplaag van de oever.

**Nieuw:**

> Cluster 5 zijn enkele locaties in Assendelft met een zeer hoge P-AL in het slib, een hoog gehalte P-totaal in de oever, zeer hoge concentratie chloride in het poriewater (mediane concentratie 34 mg/l), lagere opgelost ijzergehalte (calciumchloride) en een veel hogere pH (calciumchloride) in de toplaag van de oever.

### W13 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: figuur met bijschrift
- Door: Debby van Rotterdam

**Markup:**

> Figuur 10: VIP-vergelijking XGBoost: Gain vs. permutation importance per doelvariabele, genormaliseerd binnen doelvariabelen (+ = positief, - = negatief verband met de doelvariabele){+Misschien het model voor bedekking ondergedoken waterplanten verwijderen omdat dit zo’n zwak model is? +}

**Nieuw:**

> Figuur 10: VIP-vergelijking XGBoost: Gain vs. permutation importance per doelvariabele, genormaliseerd binnen doelvariabelen (+ = positief, - = negatief verband met de doelvariabele)Misschien het model voor bedekking ondergedoken waterplanten verwijderen omdat dit zo’n zwak model is?

### W14 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Draagkracht oever wordt overwegend bepaald door fysieke kenmerken: onderholling en slibdikte zijn beide negatief gerelateerd{+. +}[- (-]{+M+}[-m-]eer onderholling en dikkere sliblagen ondermijnen de oever letterlijk, wat een fysisch aannemelijk verband is[-)-].

**Nieuw:**

> Draagkracht oever wordt overwegend bepaald door fysieke kenmerken: onderholling en slibdikte zijn beide negatief gerelateerd. Meer onderholling en dikkere sliblagen ondermijnen de oever letterlijk, wat een fysisch aannemelijk verband is.

### W15 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Slibdikte zelf hangt samen met zowel beheer als oevermorfologie{+. +}[-:-] [-e-]{+E+}en hogere koebelasting is positief gerelateerd aan slibdikte {+waarbij +}[-(-]meer vertrapping door drinkend vee [-gaat -]samen[- -]{+gaat +}met meer slib[-)-]{+.+}[-,-] [-terwijl -]{+E+}[-e-]en lage draagkracht van de oever, veel onderholling en een groter[-e-] oppervlak[-te-] afscheurende oever {+gaan +}eveneens samen[-gaan-] met méér slib{+. +}[- (d-]{+D+}it past bij een beeld van fysiek instabielere, afkalvende oevers waar meer erosie optreedt[-)-]. Een steilere taludhoek onder de waterlijn is daarentegen negatief gerelateerd aan slibdikte: hoe steiler de oever, hoe minder slib. Waarschijnlijk heeft dit ermee te maken dat gebieden met steilere taluds (en een grotere drooglegging) vaak een stevigere, stabielere oever hebben, die niet verzakt is, waardoor er minder materiaal naar het water erodeert of afkalft.{+ Een maat voor de fosfaatbeschikbaarheid in de toplaag van het slib (P-AL) heeft de sterkste relatie met slibdikte. Dit verband lijkt niet een simpel causaal verband. Het is mogelijk dat een grote slibdikte bepalend is voor de mobilsatie van fosfaat als gevolg van redox processen. Anderzijds is het mogelijk dat een hoge fosfaatbeschikbaarheid het gevolg is van intensief agrarisch beheer en relatief diepe ontwatering in het verleden. +}

**Nieuw:**

> Slibdikte zelf hangt samen met zowel beheer als oevermorfologie. Een hogere koebelasting is positief gerelateerd aan slibdikte waarbij meer vertrapping door drinkend vee samengaat met meer slib. Een lage draagkracht van de oever, veel onderholling en een groter oppervlak afscheurende oever gaan eveneens samen met méér slib. Dit past bij een beeld van fysiek instabielere, afkalvende oevers waar meer erosie optreedt. Een steilere taludhoek onder de waterlijn is daarentegen negatief gerelateerd aan slibdikte: hoe steiler de oever, hoe minder slib. Waarschijnlijk heeft dit ermee te maken dat gebieden met steilere taluds (en een grotere drooglegging) vaak een stevigere, stabielere oever hebben, die niet verzakt is, waardoor er minder materiaal naar het water erodeert of afkalft. Een maat voor de fosfaatbeschikbaarheid in de toplaag van het slib (P-AL) heeft de sterkste relatie met slibdikte. Dit verband lijkt niet een simpel causaal verband. Het is mogelijk dat een grote slibdikte bepalend is voor de mobilsatie van fosfaat als gevolg van redox processen. Anderzijds is het mogelijk dat een hoge fosfaatbeschikbaarheid het gevolg is van intensief agrarisch beheer en relatief diepe ontwatering in het verleden.

### W16 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Waterplanten (aantal soorten, bedekking ondergedoken planten, soortensamenstelling hydrofyten) hangen sterk samen met de fosfaatstatus van slib en poriewater: P-AL slib en Fe/P zijn bij alle drie doelvariabelen belangrijke voorspellers, en steeds met een negatief verband met de waterplanten-uitkomst[- -]{+Een +}[-(-]hoger P-AL of FeP [-→ -]{+leidt tot +}minder waterplantensoorten, minder bedekking, en een soortensamenstelling die verschuift[-)-]. Dit is consistent met het bekende ecologische mechanisme waarbij hogere fosfaatbeschikbaarheid in slib/poriewater algengroei en troebeling bevordert, ten koste van ondergedoken waterplanten.

**Nieuw:**

> Waterplanten (aantal soorten, bedekking ondergedoken planten, soortensamenstelling hydrofyten) hangen sterk samen met de fosfaatstatus van slib en poriewater: P-AL slib en Fe/P zijn bij alle drie doelvariabelen belangrijke voorspellers, en steeds met een negatief verband met de waterplanten-uitkomstEen hoger P-AL of FeP leidt tot minder waterplantensoorten, minder bedekking, en een soortensamenstelling die verschuift. Dit is consistent met het bekende ecologische mechanisme waarbij hogere fosfaatbeschikbaarheid in slib/poriewater algengroei en troebeling bevordert, ten koste van ondergedoken waterplanten.

### W17 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Door: Debby van Rotterdam

**Markup:**

> Meer onderholling en dikker slib gaan samen met minder waterplanten, mogelijk via een verminderde lichtdoorlating{+,+} [-of-] instabielere waterbodem{+ en/ of een chemische samenstelling die beperkend is (P, S, NH4+)+}. Opvallend is dat ammonium in het poriewater in dit model nauwelijks nog als belangrijke voorspeller voor hydrofyten en bedekking ondergedoken planten naar voren komt (in tegenstelling tot eerdere modelversies, waarin ammonium bij deze doelvariabelen hoog scoorde); ammonium speelt in de huidige VIP-figuur alleen nog een rol bij P-AL slib en oeverindex. Dit kan wijzen op een sterkere samenhang tussen ammonium en de nu prominentere P-AL/FeP-variabelen (waardoor het model bij voorkeur op die laatste splitst), of op een reële verandering in het relatieve belang van ammonium tussen modelversies (beide zijn met de huidige analyse niet van elkaar te onderscheiden).

**Nieuw:**

> Meer onderholling en dikker slib gaan samen met minder waterplanten, mogelijk via een verminderde lichtdoorlating, instabielere waterbodem en/ of een chemische samenstelling die beperkend is (P, S, NH4+). Opvallend is dat ammonium in het poriewater in dit model nauwelijks nog als belangrijke voorspeller voor hydrofyten en bedekking ondergedoken planten naar voren komt (in tegenstelling tot eerdere modelversies, waarin ammonium bij deze doelvariabelen hoog scoorde); ammonium speelt in de huidige VIP-figuur alleen nog een rol bij P-AL slib en oeverindex. Dit kan wijzen op een sterkere samenhang tussen ammonium en de nu prominentere P-AL/FeP-variabelen (waardoor het model bij voorkeur op die laatste splitst), of op een reële verandering in het relatieve belang van ammonium tussen modelversies (beide zijn met de huidige analyse niet van elkaar te onderscheiden).

### W18 - Resultaten > Validatie XGBoost-model: residuen en uitschieters

- Type: tabelrij (Tabel 5: Residudiagnostiek XGBoost per doelvariabele: R², RMSE, scheefheid (skewness) van de residuen en gemiddelde bias)
- Door: Debby van Rotterdam

**Markup:**

> P-AL slib (mg P2O5/100g) | 83.7 | 13.161 [-mg P2O5/100g-] | 0.03 | 25.16 [-mg P2O5/100g-] | -3.48 [-mg P2O5/100g-] | 123

**Nieuw:**

> P-AL slib (mg P2O5/100g) | 83.7 | 13.161 | 0.03 | 25.16 | -3.48 | 123

### W19 - Resultaten > ALE-kantelpunten van de meest betrouwbare modellen [kop verwijderd]

- Type: 28 opeenvolgende blokken volledig verwijderd (alinea's/koppen/tabelrijen)
- Door: Debby van Rotterdam

**Markup:**

> [-VERWIJDER de hele passage, vanaf: "ALE-kantelpunten van de meest betrouwbare modellen" t/m: "Samenvattend: kantelpunten die op veel sloten zijn gebaseerd én geen verhoogde spreiding hebben (Onderholling → Aantal waterplantensoorten; pH slib en Draagkracht perceel → Redox slib; FeP → P-AL slib) zijn het meest geschikt om als concreet omslagpu ..."-]

**Nieuw:**

> 

## Opmerkingen

### O1 - Methoden > Data en voorspellende variabelen

- Type: tabelrij (Tabel 1: Overzicht van gemodelleerde doelvariabelen)
- Word-id: 10

**Gemarkeerde tekst:**

> Variabele

**Context (volledige alinea/rij):**

> Variabele | Nederlandse naam | Eenheid

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Verschillende variabelen zijn in meerdere zones gemeten. Moet dit worden gesprecificeerd? Draagkracht diepte?

### O2 - Methoden > Data en voorspellende variabelen > Toegepaste datatransformaties of standaardisaties > Kengetallen op basis van profielen

- Type: alinea
- Word-id: 16

**Gemarkeerde tekst:**

> Drooglegging oever en perceel: zowel het hoogste punt op de oever als het maaiveld minus jaarrondstreefpeil (NHI)

**Context (volledige alinea/rij):**

> Drooglegging oever en perceel: zowel het hoogste punt op de oever als het maaiveld minus jaarrondstreefpeil (NHI)

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Waarom niet tov het gemeten waterpeil? Hoe ga je er mee om als peilen varieren tussen zomer en winter?

### O3 - Methoden > Data en voorspellende variabelen > Toegepaste datatransformaties of standaardisaties > Koebelasting drinkende koeien

- Type: alinea
- Word-id: 22

**Gemarkeerde tekst:**

> de natte omtrek

**Context (volledige alinea/rij):**

> waarbij de natte omtrek van de oever een maat is voor hoeveel slootoever daadwerkelijk bereikbaar is voor drinkende koeien. Vrij-tekstwaarden voor drinkgedrag en drinkbakaanwezigheid (bijv. “ja”/“nee”/“onbekend”-varianten) zijn eerst genormaliseerd naar TRUE/FALSE/NA.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Note to self: Check even hoe dit uitpakt in eenheden

### O4 - Methoden > Data en voorspellende variabelen > Toegepaste datatransformaties of standaardisaties > Erosieindex en vernattingsrisico-index

- Type: alinea
- Word-id: 24

**Gemarkeerde tekst:**

> Verhouding waterdiepte/slibdikte

**Context (volledige alinea/rij):**

> Verhouding waterdiepte/slibdikte, genormaliseerd op het 95e-percentiel van deze verhouding en vervolgens geïnverteerd: een lage verhouding (veel slib ten opzichte van waterdiepte) duidt op meer erosie/sedimentatie.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Als je het inverteerd kun je er voor de duidelijkheid ook slibdikte/ waterdiepte van maken?

### O5 - Methoden > Clusteranalyse abiotische variabelen > Relatie tussen fosfor- en ijzervariabelen

- Type: alinea
- Word-id: 30

**Gemarkeerde tekst:**

> deze P-variabelen met een log10-transformatie behandeld

**Context (volledige alinea/rij):**

> Uit deze matrix (en de bijbehorende Pearson-correlaties in Figuur 31 in de bijlage) blijkt dat sommige P-variabelen onderling sterker correleren dan met de Fe-variabelen, en dat de Fe/P-ratio’s vooral door de Fe-concentratie worden gedreven (zie ook Tabel 14, waarin de Fe/P-ratio op oeverdiepte 0–25 cm om die reden is uitgesloten ten gunste van de losse Fe-maat). Een deel van de P-variabelen (totaal-P in slib, P-AL en P2O5-XRF op oeverdiepte 0–25 cm) bleek daarnaast op slootniveau sterk gecorreleerd (Pearson r doorgaans > 0.8), maar deze correlatie werd grotendeels bepaald door een klein aantal sloten met sterk verhoogde P- waarden en chloride in het poriewater (“hotspots in Assendelft”): de rangcorrelatie (Spearman) tussen dezelfde variabelen lag duidelijk lager (doorgaans 0.4–0.5), wat erop wijst dat de samenhang buiten deze hotspot-sloten veel zwakker is; Hoge totaal-P gehalten in de oever (de veenpercelen) hebben geen sterke relatie met de P-AL of P in het poriewater in het slib (Spearman r < 0.24 en < 0.001). Om te voorkomen dat deze hotspots de clustering onevenredig sterk zouden sturen via de Euclidische afstand die k-means gebruikt, zijn deze P-variabelen met een log10-transformatie behandeld (zie hierboven); dit vermindert de invloed van de hotspot-sloten op de clustering zelf, niet alleen op de visuele weergave in Figuur 6.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Voor allen analyses of alleen voor de analyse die gevoelig zijn voor outliers?

### O6 - Methoden > Vergelijking meting met veensloottypen (clusteranalyse geografische data verkenningsfase)

- Type: kop (niveau 2)
- Word-id: 32

**Gemarkeerde tekst:**

> veensloottypen

**Context (volledige alinea/rij):**

> Vergelijking meting met veensloottypen (clusteranalyse geografische data verkenningsfase)

**Opmerking:**

- **FLORON** (2026-10-06): Ook in t rapport aangeven dat deze typering dus echt anders is dan de typering op basis van data in verkenningsfase (dus met alleen kaartdata kan je een sloot niet goed typeren). Voorbeeld van slootpeil (drooglegging) uitwerken. Conlusie is dat je buiten moet kijken!

### O7 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters

- Type: kop (niveau 2)
- Word-id: 47

**Gemarkeerde tekst:**

> Overeenkomst veensloottypen op basis van ruimtelijke

**Context (volledige alinea/rij):**

> Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters

**Opmerking:**

- **FLORON** (2026-10-06): Ook in t rapport aangeven dat deze typering dus echt anders is dan de typering op basis van data in verkenningsfase (dus met alleen kaartdata kan je een sloot niet goed typeren). Voorbeeld van slootpeil (drooglegging) uitwerken. Conlusie is dat je buiten moet kijken!

### O8 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters

- Type: alinea
- Word-id: 52

**Gemarkeerde tekst:**

> Hoge OS in VeeST: de veldmetingen hebben structureel hogere OS-waarden (mediaan 36%) dan

**Context (volledige alinea/rij):**

> Hoge OS in VeeST: de veldmetingen hebben structureel hogere OS-waarden (mediaan 36%) dan de GIS-laag (mediaan 21%). Cluster 2 heeft in de GIS-indeling de hoogste OS-waarde (30%) en trekt daardoor de meeste VeeST-locaties aan.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): De clustering is gebaseerd op Bodemschat en die smoothed hoge waarden.

### O9 - Resultaten > Overeenkomst veensloottypen op basis van ruimtelijke clusters vs. variabele-gebaseerde clusters > Schaalbereik GIS-laag vs. veldmeting

- Type: alinea
- Word-id: 58

**Gemarkeerde tekst:**

> Het verschil tussen de NOBV zomerdrooglegging (~0.47 m) en de VeeST veldmeting (~0.33 m) bedraagt ~0.14 m. Dit is consistent met het verschil in maaiveld (GIS − VeeST: +0.34 m) minus het verschil in peil (GIS − VeeST: +0.20 m): 0.34 − 0.20 = 0.14 m, wat bevestigt dat de meting intern consistent is.

**Context (volledige alinea/rij):**

> Het verschil tussen de NOBV zomerdrooglegging (~0.47 m) en de VeeST veldmeting (~0.33 m) bedraagt ~0.14 m. Dit is consistent met het verschil in maaiveld (GIS − VeeST: +0.34 m) minus het verschil in peil (GIS − VeeST: +0.20 m): 0.34 − 0.20 = 0.14 m, wat bevestigt dat de meting intern consistent is.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Ok ik ben nogal moe, maar dit volg ik niet.

### O10 - Resultaten > Modelprestaties op de testset XGBoost vs. Random Forest

- Type: alinea
- Word-id: 84

**Gemarkeerde tekst:**

> Tabel 3: Testset R² (%) en ruimtelijke CV-R² (%) voor XGBoost en Random Forest per doelvariabele. CV-R² is het gemiddelde over leave-one-waterschap-out folds met ≥ 4 testlocaties (zie Tabel 6); NA betekent dat de doelvariabele niet in de ruimtelijke CV is meegenomen.Doelvariabele | R² XGBoost (%) | RMSE XGBoost | R² RF (%) | RMSE RF | Beste model | CV R² XGBoost (%) | CV R² RF (%)P-AL slib (mg P2O ...

**Context (volledige alinea/rij):**

> Tabel 3: Testset R² (%) en ruimtelijke CV-R² (%) voor XGBoost en Random Forest per doelvariabele. CV-R² is het gemiddelde over leave-one-waterschap-out folds met ≥ 4 testlocaties (zie Tabel 6); NA betekent dat de doelvariabele niet in de ruimtelijke CV is meegenomen.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Aantal cijfers significant afbakenen

### O11 - Resultaten > Modelprestaties op de testset XGBoost vs. Random Forest

- Type: alinea
- Word-id: 87

**Gemarkeerde tekst:**

> Forest

**Context (volledige alinea/rij):**

> Tabel 4: Doelvariabelen met het grootste verschil tussen testset-R² XGBoost en Random Forest

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Waarom mist P-Al hier?
  - Reactie **Debby van Rotterdam** (2026-10-01): Weer aantal cijfers significant beperken

### O12 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Word-id: 90

**Gemarkeerde tekst:**

> richting variabelen met veel unieke waarden

**Context (volledige alinea/rij):**

> Omdat XGBoost bij de meerderheid van de doelvariabelen beter presteert dan Random Forest (zie hierboven), worden de belangrijkste voorspellende variabelen uit dit model getoond, met twee complementaire maten als gegroepeerde balken naast elkaar: de ingebouwde Gain-maatstaf (informatiewinst per splitsing op die variabele, opgeteld over alle bomen) en permutation importance op de validatieset (afname in voorspelkracht wanneer de waarden van een voorspellende variabele willekeurig worden geschud). Beide maten zijn per doelvariabele genormaliseerd naar het hoogst scorende kenmerk (1 = hoogste belang binnen die maat); 0% betekent dat een kenmerk niet in de top-10 van die methode voorkomt. Permutation importance is minder gevoelig voor bias richting variabelen met veel unieke waarden of niveaus dan Gain. Het +/--label geeft de richting van het Pearson-verband tussen voorspeller en doelvariabele weer.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Wat bedoel je daar precies mee?

### O13 - Resultaten > Voorspellers die het meest bijdragen aan de modelprestaties

- Type: alinea
- Word-id: 124

**Gemarkeerde tekst:**

> Waarschijnlijk heeft dit ermee te maken dat gebieden met steilere taluds (en een grotere drooglegging) vaak een stevigere, stabielere oever hebben, die niet verzakt is, waardoor er minder materiaal naar het water erodeert of afkalft.

**Context (volledige alinea/rij):**

> Slibdikte zelf hangt samen met zowel beheer als oevermorfologie. Een hogere koebelasting is positief gerelateerd aan slibdikte waarbij meer vertrapping door drinkend vee samengaat met meer slib. Een lage draagkracht van de oever, veel onderholling en een groter oppervlak afscheurende oever gaan eveneens samen met méér slib. Dit past bij een beeld van fysiek instabielere, afkalvende oevers waar meer erosie optreedt. Een steilere taludhoek onder de waterlijn is daarentegen negatief gerelateerd aan slibdikte: hoe steiler de oever, hoe minder slib. Waarschijnlijk heeft dit ermee te maken dat gebieden met steilere taluds (en een grotere drooglegging) vaak een stevigere, stabielere oever hebben, die niet verzakt is, waardoor er minder materiaal naar het water erodeert of afkalft. Een maat voor de fosfaatbeschikbaarheid in de toplaag van het slib (P-AL) heeft de sterkste relatie met slibdikte. Dit verband lijkt niet een simpel causaal verband. Het is mogelijk dat een grote slibdikte bepalend is voor de mobilsatie van fosfaat als gevolg van redox processen. Anderzijds is het mogelijk dat een hoge fosfaatbeschikbaarheid het gevolg is van intensief agrarisch beheer en relatief diepe ontwatering in het verleden.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-01): Kan ook zijn dat afkalving wordt veroorzaakt door stroming. Vaak uit zich dt juist in steile kanten. Bij veel stroming wordt het slib getransporteerd/ weggespoeld

### O14 - Resultaten > ALE-kantelpunten van de meest betrouwbare modellen

- Type: figuur met bijschrift
- Word-id: 147

**Gemarkeerde tekst:**

> (geen selectie; opmerking op een punt)

**Context (volledige alinea/rij):**

> Figuur 16: ALE-kantelpunten van de belangrijkste voorspellers voor de zes meest betrouwbare XGBoost-modellen, per doelvariabele in een eigen (gekleurde) rij. De roze stippellijn markeert het omslagpunt; de subtitle geeft aan of dit een plotselinge sprong of een geleidelijke verandering betreft.

**Opmerking:**

- **Debby van Rotterdam** (2026-10-02): De kleur is vermoedelijk representatief voor de doelvariabele. Welke kleur is welke doelvariabele?

### O15 - Resultaten > ALE-kantelpunten van de meest betrouwbare modellen [kop verwijderd]

- Type: figuur met bijschrift
- Word-id: 250

**Gemarkeerde tekst:**

> (geen selectie; opmerking op een punt)

**Context (volledige alinea/rij):**

> Figuur 16: ALE-kantelpunten van de belangrijkste voorspellers voor de zes meest betrouwbare XGBoost-modellen, per doelvariabele in een eigen (gekleurde) rij. De roze stippellijn markeert het omslagpunt; de subtitle geeft aan of dit een plotselinge sprong of een geleidelijke verandering betreft. [verwijderd]

**Opmerking:**

- **Debby van Rotterdam** (2026-10-02): De kleur is vermoedelijk representatief voor de doelvariabele. Welke kleur is welke doelvariabele?

### O16 - Resultaten > Effect van beheervariabelen op erosie- en vernattingsrisico

- Type: kop (niveau 2)
- Word-id: 564

**Gemarkeerde tekst:**

> erosie- en vernattingsrisico

**Context (volledige alinea/rij):**

> Effect van beheervariabelen op erosie- en vernattingsrisico

**Opmerking:**

- **FLORON** (2026-10-06): Ook een vergelijkbaar hoofdstuk maken voor risico op BKG-emissies

