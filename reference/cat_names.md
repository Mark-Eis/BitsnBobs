# Concatenate First Names and Surnames in a Data Frame into Delimiter-Separated Character Strings

Concatenates first names and surnames contained in individual columns of
a data frame. The resulting first name-surname pairs are themselves
further combined into one or more delimiter-separated character strings
on the basis of selected grouping variables.

## Usage

``` r
cat_names(
  data,
  firstname = Firstname,
  surname = Surname,
  ...,
  .delimiter = ", "
)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a tibble).

- firstname:

  quoted name of column containing first names; default `Firstname`.

- surname:

  quoted name of column containing surnames; default `Surname`.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  names of variables to group by.

- .delimiter:

  a character string to separate the first name-surname pairs, see
  `collapse` argument of [`paste`](https://rdrr.io/r/base/paste.html);
  default `", "`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
data frame containing the column `Names` and further individual columns
for each grouping variable.

## Details

A number of first name-surname pairs in `data` are all concatenated in a
single string for each of one or more grouping variables selected using
`...`. Grouping variables in `.data` are selected using the `...`
argument with the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of **selection helpers**.

By default, the name pairs within a string are separated using commas or
otherwise using `.delimiter`, if provided.

## See also

[`paste`](https://rdrr.io/r/base/paste.html),
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>.

## Examples

``` r

starwars2 ## Has name column of original starwars split into Firstname and Surname
#> # A tibble: 63 × 12
#>    Firstname Surname     height  mass hair_color skin_color eye_color birth_year
#>    <chr>     <chr>        <int> <dbl> <chr>      <chr>      <chr>          <dbl>
#>  1 Luke      Skywalker      172    77 blond      fair       blue            19  
#>  2 Darth     Vader          202   136 none       white      yellow          41.9
#>  3 Leia      Organa         150    49 brown      light      brown           19  
#>  4 Owen      Lars           178   120 brown, gr… light      blue            52  
#>  5 Beru      Whitesun L…    165    75 brown      light      blue            47  
#>  6 Biggs     Darklighter    183    84 black      light      brown           24  
#>  7 Obi-Wan   Kenobi         182    77 auburn, w… fair       blue-gray       57  
#>  8 Anakin    Skywalker      188    84 blond      fair       blue            41.9
#>  9 Wilhuff   Tarkin         180    NA auburn, g… fair       blue            64  
#> 10 Han       Solo           180    80 brown      fair       brown           29  
#> # ℹ 53 more rows
#> # ℹ 4 more variables: sex <chr>, gender <chr>, homeworld <chr>, species <chr>

starwars2 |>
  cat_names(Firstname, Surname, homeworld) |>
  print_all()
#> # A tibble: 38 × 2
#>    homeworld      Names                                                         
#>    <chr>          <chr>                                                         
#>  1 Alderaan       Leia Organa, Bail Prestor Organa, Raymus Antilles             
#>  2 Aleen Minor    Ratts Tyerel                                                  
#>  3 Bestine IV     Jek Tono Porkins                                              
#>  4 Cato Neimoidia Nute Gunray                                                   
#>  5 Champala       Mas Amedda                                                    
#>  6 Chandrila      Mon Mothma                                                    
#>  7 Concord Dawn   Jango Fett                                                    
#>  8 Corellia       Han Solo, Wedge Antilles                                      
#>  9 Coruscant      Finis Valorum, Adi Gallia, Jocasta Nu                         
#> 10 Dathomir       Darth Maul                                                    
#> 11 Dorin          Plo Koon                                                      
#> 12 Endor          Wicket Systri Warrick                                         
#> 13 Eriadu         Wilhuff Tarkin                                                
#> 14 Geonosis       Poggle the Lesser                                             
#> 15 Glee Anselm    Kit Fisto                                                     
#> 16 Haruun Kal     Mace Windu                                                    
#> 17 Iktotch        Saesee Tiin                                                   
#> 18 Iridonia       Eeth Koth                                                     
#> 19 Kamino         Boba Fett, Lama Su, Taun We                                   
#> 20 Mirial         Luminara Unduli, Barriss Offee                                
#> 21 Muunilinst     San Hill                                                      
#> 22 Naboo          Padmé Amidala, Jar Jar Binks, Roos Tarpals, Rugor Nass, Ric O…
#> 23 Nal Hutta      Jabba Desilijic Tiure                                         
#> 24 Ojom           Dexter Jettster                                               
#> 25 Quermia        Yarael Poof                                                   
#> 26 Ryloth         Bib Fortuna, Ayla Secura                                      
#> 27 Shili          Shaak Ti                                                      
#> 28 Skako          Wat Tambor                                                    
#> 29 Socorro        Lando Calrissian                                              
#> 30 Stewjon        Obi-Wan Kenobi                                                
#> 31 Sullust        Nien Nunb                                                     
#> 32 Tatooine       Luke Skywalker, Darth Vader, Owen Lars, Beru Whitesun Lars, B…
#> 33 Tund           Ben Quadinaros                                                
#> 34 Umbara         Sly Moore                                                     
#> 35 Utapau         Tion Medon                                                    
#> 36 Vulpter        Dud Bolt                                                      
#> 37 Zolan          Zam Wesell                                                    
#> 38 NA             Arvel Crynyd, Qui-Gon Jinn, Poe Dameron, Captain Phasma       

starwars2 |>
  cat_names(,, species, .delimiter = "; ") |>
  print_all()
#> # A tibble: 27 × 2
#>    species    Names                                                             
#>    <chr>      <chr>                                                             
#>  1 Aleena     Ratts Tyerel                                                      
#>  2 Besalisk   Dexter Jettster                                                   
#>  3 Chagrian   Mas Amedda                                                        
#>  4 Clawdite   Zam Wesell                                                        
#>  5 Ewok       Wicket Systri Warrick                                             
#>  6 Geonosian  Poggle the Lesser                                                 
#>  7 Gungan     Jar Jar Binks; Roos Tarpals; Rugor Nass                           
#>  8 Human      Luke Skywalker; Darth Vader; Leia Organa; Owen Lars; Beru Whitesu…
#>  9 Hutt       Jabba Desilijic Tiure                                             
#> 10 Iktotchi   Saesee Tiin                                                       
#> 11 Kaminoan   Lama Su; Taun We                                                  
#> 12 Kel Dor    Plo Koon                                                          
#> 13 Mirialan   Luminara Unduli; Barriss Offee                                    
#> 14 Muun       San Hill                                                          
#> 15 Nautolan   Kit Fisto                                                         
#> 16 Neimodian  Nute Gunray                                                       
#> 17 Pau'an     Tion Medon                                                        
#> 18 Quermian   Yarael Poof                                                       
#> 19 Skakoan    Wat Tambor                                                        
#> 20 Sullustan  Nien Nunb                                                         
#> 21 Tholothian Adi Gallia                                                        
#> 22 Togruta    Shaak Ti                                                          
#> 23 Toong      Ben Quadinaros                                                    
#> 24 Twi'lek    Bib Fortuna; Ayla Secura                                          
#> 25 Vulptereen Dud Bolt                                                          
#> 26 Zabrak     Darth Maul; Eeth Koth                                             
#> 27 NA         Jek Tono Porkins; Gregar Typho; Sly Moore                         

starwars2 |>
  cat_names(,, homeworld, species) |>
  print_all()
#> # A tibble: 42 × 3
#>    homeworld      species    Names                                              
#>    <chr>          <chr>      <chr>                                              
#>  1 Alderaan       Human      Leia Organa, Bail Prestor Organa, Raymus Antilles  
#>  2 Aleen Minor    Aleena     Ratts Tyerel                                       
#>  3 Bestine IV     NA         Jek Tono Porkins                                   
#>  4 Cato Neimoidia Neimodian  Nute Gunray                                        
#>  5 Champala       Chagrian   Mas Amedda                                         
#>  6 Chandrila      Human      Mon Mothma                                         
#>  7 Concord Dawn   Human      Jango Fett                                         
#>  8 Corellia       Human      Han Solo, Wedge Antilles                           
#>  9 Coruscant      Human      Finis Valorum, Jocasta Nu                          
#> 10 Coruscant      Tholothian Adi Gallia                                         
#> 11 Dathomir       Zabrak     Darth Maul                                         
#> 12 Dorin          Kel Dor    Plo Koon                                           
#> 13 Endor          Ewok       Wicket Systri Warrick                              
#> 14 Eriadu         Human      Wilhuff Tarkin                                     
#> 15 Geonosis       Geonosian  Poggle the Lesser                                  
#> 16 Glee Anselm    Nautolan   Kit Fisto                                          
#> 17 Haruun Kal     Human      Mace Windu                                         
#> 18 Iktotch        Iktotchi   Saesee Tiin                                        
#> 19 Iridonia       Zabrak     Eeth Koth                                          
#> 20 Kamino         Human      Boba Fett                                          
#> 21 Kamino         Kaminoan   Lama Su, Taun We                                   
#> 22 Mirial         Mirialan   Luminara Unduli, Barriss Offee                     
#> 23 Muunilinst     Muun       San Hill                                           
#> 24 Naboo          Gungan     Jar Jar Binks, Roos Tarpals, Rugor Nass            
#> 25 Naboo          Human      Padmé Amidala, Ric Olié, Quarsh Panaka             
#> 26 Naboo          NA         Gregar Typho                                       
#> 27 Nal Hutta      Hutt       Jabba Desilijic Tiure                              
#> 28 Ojom           Besalisk   Dexter Jettster                                    
#> 29 Quermia        Quermian   Yarael Poof                                        
#> 30 Ryloth         Twi'lek    Bib Fortuna, Ayla Secura                           
#> 31 Shili          Togruta    Shaak Ti                                           
#> 32 Skako          Skakoan    Wat Tambor                                         
#> 33 Socorro        Human      Lando Calrissian                                   
#> 34 Stewjon        Human      Obi-Wan Kenobi                                     
#> 35 Sullust        Sullustan  Nien Nunb                                          
#> 36 Tatooine       Human      Luke Skywalker, Darth Vader, Owen Lars, Beru White…
#> 37 Tund           Toong      Ben Quadinaros                                     
#> 38 Umbara         NA         Sly Moore                                          
#> 39 Utapau         Pau'an     Tion Medon                                         
#> 40 Vulpter        Vulptereen Dud Bolt                                           
#> 41 Zolan          Clawdite   Zam Wesell                                         
#> 42 NA             Human      Arvel Crynyd, Qui-Gon Jinn, Poe Dameron, Captain P…
```
