# Final Data Science Project: `legislatoR+` <img src="https://raw.githubusercontent.com/saschagobel/legislatoR/master/images/sticker.jpg" width="120" align="right" />

## Link to [`Shiny App`](https://687qak-gamesluxx.shinyapps.io/legislatoRplus/)

## Link to [Project Report](https://raw.githack.com/intro-to-data-science-21/data-project-legislatoRplus/main/project_report/project_report.html?token=ALMDJMZ7M5IJ46OQBKQANLDBX6MMG)

</br>

---

</br>

## Project Members <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Hertie_School_of_Governance_logo.svg/1280px-Hertie_School_of_Governance_logo.svg.png" width="240" align="right" />

- Benedikt Ströbl ([Twitter](https://twitter.com/benediktstroebl), [LinkedIn](https://www.linkedin.com/in/benedikt-ströbl-026926173/))
- Lukas Warode ([Website](https://lwarode.github.io/), [Twitter](https://twitter.com/lukas_warode), [LinkedIn](https://www.linkedin.com/in/lukas-warode-64a84a1a3/))

## Project Description

`legislatoR+` is a dashboard project from Benedikt Ströbl and Lukas Warode with the goal to provide an accessible and useful visual tool for the existing `legislatoR` R package. `legislatoR` itself is based on *The Comparative Legislators Database* (Göbel and Munzert, 2021), a far-reaching political science data project that provides both comprehensive information for over 45,000 politicians from ten countries while also including linking possibilities to other research databases within the political science context.

`legislatoR+` provides (in the preliminary version) several options of visual communication, including a constituency-based map and 2 descriptive plots. All graphical elements are based on `Shiny's` reactive infrastructure.

For reasons of accessibility and simplicity that comes with avoiding large data handling issues -- the scope of available data differs across the ten countries -- the preliminary version for the final project is based on a German sample starting with the 15th legislative period (*Bundestagswahl 2002*).

## Statement of Contribution

Most of the project work took place in a very collaborative dynamic. In the preliminary stages, there was a lively exchange between both project members, with most of the ideas and theoretical approaches of the project having been implicitly worked on jointly.

In the advanced and practical stages, a certain division of labor crystallized. However, a large part of the project was approached in a co-working place, which is why many practical projects and especially problems were evaluated directly together.

In detail, the division of work was as follows:

- __Benedikt Ströbl__ mainly handled the different map data formats and took care of the configuration of the HTML template. He also created the visual `leaflet` map base and interactivity with special emphasis on the constituencies and the implied complex data matching and wrangling tasks inside of the `Shiny` backend context.

- __Lukas Warode__ was mainly involved in setting up the `Shiny` backend and did a lot of the practical data manipulation inside and outside the `Shiny` reactivity context. He also worked on the `ggplot2` foundation of the two descriptive statistics and configured the visual and written outline of the `R Markdown` report.

---

## License

The material in this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php). 
