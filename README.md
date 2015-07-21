This repository contains code to build cosponsorship networks from bills passed in of the [Slovakian Parliament](http://www.nrsr.sk/) .

- [interactive demo](http://briatte.org/nrsr)
- [static plots](http://briatte.org/nrsr/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors (using a pretty inefficient method that works, despite the huge pain of having to deal with ASP 'doPostBack' forms). All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature number
- `id` -- bill master id
- `cpt` -- bill legislature-specific id
- `date` -- date of introduction of the bill (yyyy-mm-dd)
- `title` -- title
- `status` -- bill status
- `authors` -- bill sponsors, using standardized names

The sponsor names are standardized by converting them to ASCII, which moves all diacritics before or after the letter (e.g. "L. Dzur'ak"). This is necessary to match the bills dataset to the sponsors dataset.

## Sponsors

The sponsors data have multiple entries for each sponsor (one per legislature in which the sponsor sat).

- `legislature` -- legislature of activity
- `id` -- numeric id; points to the profile URL
- `name` -- full name, without titles like "PhDr." or "Ing."
- `nyears` -- seniority (time in office since legislature 1), in years
- `title` -- stuff like "PhDr." or "Ing."
- `first` -- first name
- `last` -- last name
- `party` -- political party, abbreviated
- `born` -- year of birth
- `place` -- place of residence
- `county` -- constituency (many missing in legislature 2, full otherwise)
- `photo` -- photo URL, a variation of the profile URL stored in `id`
- `sex` -- gender (F/M), imputed from first and family names ('á')
- `uid` -- standardized name that matches those in the bill listings

Notes -- constituencies are standardized to Wikipedia English handles, and genders have been checked to ensure that males with a final 'a' in their first name are coded as males (e.g. Béla Bugár).
