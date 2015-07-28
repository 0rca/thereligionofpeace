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

`/cities` - list of cities

`/cities/:city` - list of attacks in the city

`/countries` - list of countries

`/countries/:country` - list of attacks for the `country`

`/countries/:country/:city` - narrow search to a particular city within a
country

