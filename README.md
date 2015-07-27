Extracts data from [the religion of peace](http://thereligionofpeace.com) html
pages and serves them as json

# Building
Install `stack` from [Stackage](http://www.stackage.org/)

```sh
stack build
```

# Using

Run `stack exec thereligionofpeace resources/*.html` and go to [this page](http://localhost:3000) to try it out.


# API

`/countries` - list of countries

`/cities` - same for cities

`/:country` - list of crimes for the `country`

`/:country/:city` - same for a particular city

