# Shared Vendors

This folder contains shared libraries which come from external vendors. The code was pulled from the following repos and branches:

- SQLiteCpp: https://github.com/SRombauts/SQLiteCpp
- sqlite3: https://sqlite.org/index.html
- googletest: https://github.com/google/googletest
- utf8rewind: https://bitbucket.org/knight666/utf8rewind (mercurial repo, not git).
- RapidXml: http://rapidxml.sourceforge.net
- json: https://github.com/nlohmann/json

## Sparkle

Our version of Sparkle is hosted at `https://github.com/sparkle-project/Sparkle.git` and on Mac is compiled using the following commands:

    make release
    
Once the build is finished the build directory will open in the finder. The build product is the zip file found in that directory.
