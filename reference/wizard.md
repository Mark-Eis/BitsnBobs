# Extract Unique Values from Data Frame Columns, Sort and Concatenate

`wizard()` extracts and sorts unique values of a selected column from a
data frame, and optionally pastes into a character string.

`data_wizard()` does the same across all columns in a data frame.

## Usage

``` r
wizard(data, col, .collapse = NULL, .noquote = FALSE)

data_wizard(data, .collapse = NULL, .noquote = TRUE)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a tibble).

- col:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of character column to extract.

- .collapse:

  an optional character string to separate the results, see
  [`paste`](https://rdrr.io/r/base/paste.html); default `NULL`.

- .noquote:

  `logical` whether to return an object of class `"noquote"`; default
  `FALSE` for `wizard()`, `TRUE` for `data_wizard()`.

## Value

For `wizard()`, a vector of the same type as `col` or a single character
string if a value for `.collapse` is supplied.

For `data_wizard()`, a [`list`](https://rdrr.io/r/base/list.html)
corresponding to columns in `data` that are of
[`atomic`](https://rdrr.io/r/base/vector.html) types, with list elements
as for `wizard()`.

## Details

`wizard()` can be useful within a piped sequence to quickly extract or
review [`sorted`](https://rdrr.io/r/base/sort.html),
[`unique`](https://rdrr.io/r/base/unique.html) contents of a column and
optionally collapse into a single character string using
[`paste`](https://rdrr.io/r/base/paste.html) by providing a suitable
value for `.collapse`.

`data_wizard()` invokes `wizard()` for all columns in `data` that are
one of the [`atomic`](https://rdrr.io/r/base/vector.html) types, ignores
columns of other types and shows a warning if any present. If
`.collapse` is used with `data_wizard()`, all columns are returned as
`character vectors`.

Further information at [Using
wizard](https://mark-eis.github.io/BitsnBobs/articles/Using-wizard.html).

## See also

[`paste`](https://rdrr.io/r/base/paste.html),
[`sort`](https://rdrr.io/r/base/sort.html),
[`unique`](https://rdrr.io/r/base/unique.html).

Other detective:
[`detective()`](https://mark-eis.github.io/BitsnBobs/reference/detective.md),
[`retriever()`](https://mark-eis.github.io/BitsnBobs/reference/retriever.md)

## Examples

``` r
starwars |> wizard(homeworld)
#>  [1] "Alderaan"       "Aleen Minor"    "Bespin"         "Bestine IV"    
#>  [5] "Cato Neimoidia" "Cerea"          "Champala"       "Chandrila"     
#>  [9] "Concord Dawn"   "Corellia"       "Coruscant"      "Dathomir"      
#> [13] "Dorin"          "Endor"          "Eriadu"         "Geonosis"      
#> [17] "Glee Anselm"    "Haruun Kal"     "Iktotch"        "Iridonia"      
#> [21] "Kalee"          "Kamino"         "Kashyyyk"       "Malastare"     
#> [25] "Mirial"         "Mon Cala"       "Muunilinst"     "Naboo"         
#> [29] "Nal Hutta"      "Ojom"           "Quermia"        "Rodia"         
#> [33] "Ryloth"         "Serenno"        "Shili"          "Skako"         
#> [37] "Socorro"        "Stewjon"        "Sullust"        "Tatooine"      
#> [41] "Toydaria"       "Trandosha"      "Troiken"        "Tund"          
#> [45] "Umbara"         "Utapau"         "Vulpter"        "Zolan"         
#> [49] NA              
starwars |> wizard(homeworld, ", ")
#> [1] "Alderaan, Aleen Minor, Bespin, Bestine IV, Cato Neimoidia, Cerea, Champala, Chandrila, Concord Dawn, Corellia, Coruscant, Dathomir, Dorin, Endor, Eriadu, Geonosis, Glee Anselm, Haruun Kal, Iktotch, Iridonia, Kalee, Kamino, Kashyyyk, Malastare, Mirial, Mon Cala, Muunilinst, Naboo, Nal Hutta, Ojom, Quermia, Rodia, Ryloth, Serenno, Shili, Skako, Socorro, Stewjon, Sullust, Tatooine, Toydaria, Trandosha, Troiken, Tund, Umbara, Utapau, Vulpter, Zolan, NA"
starwars |> wizard(homeworld, ", ", TRUE)
#> [1] Alderaan, Aleen Minor, Bespin, Bestine IV, Cato Neimoidia, Cerea, Champala, Chandrila, Concord Dawn, Corellia, Coruscant, Dathomir, Dorin, Endor, Eriadu, Geonosis, Glee Anselm, Haruun Kal, Iktotch, Iridonia, Kalee, Kamino, Kashyyyk, Malastare, Mirial, Mon Cala, Muunilinst, Naboo, Nal Hutta, Ojom, Quermia, Rodia, Ryloth, Serenno, Shili, Skako, Socorro, Stewjon, Sullust, Tatooine, Toydaria, Trandosha, Troiken, Tund, Umbara, Utapau, Vulpter, Zolan, NA
starwars |> wizard(homeworld, "\t") |> cat()
#> Alderaan Aleen Minor Bespin  Bestine IV  Cato Neimoidia  Cerea   Champala    Chandrila   Concord Dawn    Corellia    Coruscant   Dathomir    Dorin   Endor   Eriadu  Geonosis    Glee Anselm Haruun Kal  Iktotch Iridonia    Kalee   Kamino  Kashyyyk    Malastare   Mirial  Mon Cala    Muunilinst  Naboo   Nal Hutta   Ojom    Quermia Rodia   Ryloth  Serenno Shili   Skako   Socorro Stewjon Sullust Tatooine    Toydaria    Trandosha   Troiken Tund    Umbara  Utapau  Vulpter Zolan   NA
starwars |> wizard(homeworld, "\n") |> cat()
#> Alderaan
#> Aleen Minor
#> Bespin
#> Bestine IV
#> Cato Neimoidia
#> Cerea
#> Champala
#> Chandrila
#> Concord Dawn
#> Corellia
#> Coruscant
#> Dathomir
#> Dorin
#> Endor
#> Eriadu
#> Geonosis
#> Glee Anselm
#> Haruun Kal
#> Iktotch
#> Iridonia
#> Kalee
#> Kamino
#> Kashyyyk
#> Malastare
#> Mirial
#> Mon Cala
#> Muunilinst
#> Naboo
#> Nal Hutta
#> Ojom
#> Quermia
#> Rodia
#> Ryloth
#> Serenno
#> Shili
#> Skako
#> Socorro
#> Stewjon
#> Sullust
#> Tatooine
#> Toydaria
#> Trandosha
#> Troiken
#> Tund
#> Umbara
#> Utapau
#> Vulpter
#> Zolan
#> NA

data_wizard(mtcars) 
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] 4 6 8
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335
#> 
#> $drat
#>  [1] 2.76 2.93 3.00 3.07 3.08 3.15 3.21 3.23 3.54 3.62 3.69 3.70 3.73 3.77 3.85
#> [16] 3.90 3.92 4.08 4.11 4.22 4.43 4.93
#> 
#> $wt
#>  [1] 1.513 1.615 1.835 1.935 2.140 2.200 2.320 2.465 2.620 2.770 2.780 2.875
#> [13] 3.150 3.170 3.190 3.215 3.435 3.440 3.460 3.520 3.570 3.730 3.780 3.840
#> [25] 3.845 4.070 5.250 5.345 5.424
#> 
#> $qsec
#>  [1] 14.50 14.60 15.41 15.50 15.84 16.46 16.70 16.87 16.90 17.02 17.05 17.30
#> [13] 17.40 17.42 17.60 17.82 17.98 18.00 18.30 18.52 18.60 18.61 18.90 19.44
#> [25] 19.47 19.90 20.00 20.01 20.22 22.90
#> 
#> $vs
#> [1] 0 1
#> 
#> $am
#> [1] 0 1
#> 
#> $gear
#> [1] 3 4 5
#> 
#> $carb
#> [1] 1 2 3 4 6 8
#> 

data_wizard(starwars, ", ") 
#> Warning: Omitting `data` column(s) "films", "vehicles", "starships" with non-atomic types.
#> $name
#> [1] Ackbar, Adi Gallia, Anakin Skywalker, Arvel Crynyd, Ayla Secura, BB8, Bail Prestor Organa, Barriss Offee, Ben Quadinaros, Beru Whitesun Lars, Bib Fortuna, Biggs Darklighter, Boba Fett, Bossk, C-3PO, Captain Phasma, Chewbacca, Cliegg Lars, Cordé, Darth Maul, Darth Vader, Dexter Jettster, Dooku, Dormé, Dud Bolt, Eeth Koth, Finis Valorum, Finn, Gasgano, Greedo, Gregar Typho, Grievous, Han Solo, IG-88, Jabba Desilijic Tiure, Jango Fett, Jar Jar Binks, Jek Tono Porkins, Jocasta Nu, Ki-Adi-Mundi, Kit Fisto, Lama Su, Lando Calrissian, Leia Organa, Lobot, Luke Skywalker, Luminara Unduli, Mace Windu, Mas Amedda, Mon Mothma, Nien Nunb, Nute Gunray, Obi-Wan Kenobi, Owen Lars, Padmé Amidala, Palpatine, Plo Koon, Poe Dameron, Poggle the Lesser, Quarsh Panaka, Qui-Gon Jinn, R2-D2, R4-P17, R5-D4, Ratts Tyerel, Raymus Antilles, Rey, Ric Olié, Roos Tarpals, Rugor Nass, Saesee Tiin, San Hill, Sebulba, Shaak Ti, Shmi Skywalker, Sly Moore, Tarfful, Taun We, Tion Medon, Wat Tambor, Watto, Wedge Antilles, Wicket Systri Warrick, Wilhuff Tarkin, Yarael Poof, Yoda, Zam Wesell
#> 
#> $height
#> [1] 66, 79, 88, 94, 96, 97, 112, 122, 137, 150, 157, 160, 163, 165, 166, 167, 168, 170, 171, 172, 173, 175, 177, 178, 180, 182, 183, 184, 185, 188, 190, 191, 193, 196, 198, 200, 202, 206, 213, 216, 224, 228, 229, 234, 264, NA
#> 
#> $mass
#> [1] 15, 17, 20, 32, 40, 45, 48, 49, 50, 55, 56.2, 57, 65, 66, 68, 74, 75, 77, 78.2, 79, 80, 82, 83, 84, 85, 87, 88, 89, 90, 102, 110, 112, 113, 120, 136, 140, 159, 1358, NA
#> 
#> $hair_color
#> [1] auburn, auburn, grey, auburn, white, black, blond, blonde, brown, brown, grey, grey, none, white, NA
#> 
#> $skin_color
#> [1] blue, blue, grey, brown, brown mottle, brown, white, dark, fair, fair, green, yellow, gold, green, green, grey, green-tan, brown, grey, grey, blue, grey, green, yellow, grey, red, light, metal, mottled green, none, orange, pale, red, red, blue, white, silver, red, tan, unknown, white, white, blue, white, red, yellow
#> 
#> $eye_color
#> [1] black, blue, blue-gray, brown, dark, gold, green, yellow, hazel, orange, pink, red, red, blue, unknown, white, yellow
#> 
#> $birth_year
#> [1] 8, 15, 19, 21, 22, 24, 29, 31, 31.5, 33, 37, 40, 41, 41.9, 44, 46, 47, 48, 52, 53, 54, 57, 58, 62, 64, 66, 67, 72, 82, 91, 92, 102, 112, 200, 600, 896, NA
#> 
#> $sex
#> [1] female, hermaphroditic, male, none, NA
#> 
#> $gender
#> [1] feminine, masculine, NA
#> 
#> $homeworld
#> [1] Alderaan, Aleen Minor, Bespin, Bestine IV, Cato Neimoidia, Cerea, Champala, Chandrila, Concord Dawn, Corellia, Coruscant, Dathomir, Dorin, Endor, Eriadu, Geonosis, Glee Anselm, Haruun Kal, Iktotch, Iridonia, Kalee, Kamino, Kashyyyk, Malastare, Mirial, Mon Cala, Muunilinst, Naboo, Nal Hutta, Ojom, Quermia, Rodia, Ryloth, Serenno, Shili, Skako, Socorro, Stewjon, Sullust, Tatooine, Toydaria, Trandosha, Troiken, Tund, Umbara, Utapau, Vulpter, Zolan, NA
#> 
#> $species
#> [1] Aleena, Besalisk, Cerean, Chagrian, Clawdite, Droid, Dug, Ewok, Geonosian, Gungan, Human, Hutt, Iktotchi, Kaleesh, Kaminoan, Kel Dor, Mirialan, Mon Calamari, Muun, Nautolan, Neimodian, Pau'an, Quermian, Rodian, Skakoan, Sullustan, Tholothian, Togruta, Toong, Toydarian, Trandoshan, Twi'lek, Vulptereen, Wookiee, Xexto, Yoda's species, Zabrak, NA
#> 
```
