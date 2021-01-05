# CoreData Schema Converter

This tool takes a CoreData schema file and generates a Persistance schema that implements the schema. It can
be run periodically, or turned into a build step.

## Building the Tool

### Prerequisites:

Building the tool from source requires stack to be installed and working.
See the [getting started guide](https://haskell-lang.org/get-started)
or check out the [more detailed instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade)

### Install ghc

To install the correct GHC, try running

    cd RekordBuddy/CoreSchemaConvert
    stack setup
    
or use the "--install-ghc" flag when building.

### Compile Project

This will take a fairly long the first time, since it will have to grab a compiler and all dependencies.
Subsequent builds will be much more efficent.

    cd RekordBuddy/CoreSchemaConvert
    stack build

## Installing (optional)

If you don't expect the code generator to be changing much, one can install it on your machine at this point with

    stack install

This will put the `dpcd` tool into your path. If you don't want to install the tool (i.e., building the tool frequently) 
then it might be worth using `stack exec dpcd --` which will run it from the local directory without installing it. 

## Running the Tool

Executing the project is straight-forward. You can get the help for description of the options:

    $ stack exec dpcd -- --help

    DumpCD v0.1

    dpcd [OPTIONS]
       Tool to convert core data schema into various other things

    Common flags:
          --destination=DIR        Directory where source will be generated (pwd)
          --input=FILE             The CoreData file to read
                                   (<destination>/<directoryName>/Persistence/Models/<namespace>/<namespace>V<outversion>.xcdatamodeld/contents)
       -o --outversion=NUM         Version number to use
          --isdefault              This version is the default version
       -m --minversion=NUM         How far back can this version read successfully
          --namespace=STR          The namespace where code will be injected
          --innernamespace=STR     Inside the namespace; inject code into this an inner namespace
          --directoryname=STR      The directory where code will be placed (<namespace>)
          --emitcoredatamigration  generate types that will migrate from core data
          --emitcocoawrappers      generate wrappers for cocoa types to ease migration
          --nesting=INT            How many spaces to indent generated source (4)
       -? --help                   Display help message
       -V --version                Print version information
          --numeric-version        Print just the version number


Usually you just want to run the thing and generate a new schema after a minor change in the XML. The following will run the generator, putting the build products in ".." (the project root) and read the schema
from the specified xcdatamdoel file:

    stack exec dpcd -- --destination "../../" --isdefault --minversion=9 --outversion=9 --namespace RekordBuddy --innernamespace CollectionImplementation --directoryname=Collections/RekordBuddy/Objects/Imp --input=../../Collections/RekordBuddy/Objects/Imp/Persistence/Models/RekordBuddy.xcdatamodeld/RekordBuddyV9.xcdatamodel/contents

the 'Destination' must be added to the include path as all files in that directory will be included relative
to it. Additionally, the 'Base' and 'Persistence' directories must be discoverable from the include path as well.

note that dpcd guesses the input schema, but it can be specified
directly: `--input "../Collections/RekordBuddy/Imp/Int/Persistence/Models/RekordBuddy.xcdatamodeld/RekordBuddyV9.xcdatamodel/contents"`
