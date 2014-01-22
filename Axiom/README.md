#Axiom API

##Description
Please, place here brief description

##Directory layout
* `build` - binaries is here (`build/jar/axiom-api.jar` is compiled library)
* `config` - configs (for log4j in particular)
* `docs` - documents regarding the Axiom API
* `lib` - dependencies
* `src` - sources
* `tests` - tests written using Scalatest framework

##Build
Edit `build.properties` file and specify scala home, and path to scalatest jar, then use `ant build`

##Testing
`ant test`