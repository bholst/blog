Unnamed blog software
=====================

I developed this blog software mainly for personal use on [my own blog][blog],
but I hope it is also useful for other pages. I mainly developed it to be
adaptable to all special needs I may have in the future. Furthermore I wished
to get some more insight in the development of websites written in Haskell.

Acknowledgement
---------------

This software is written in [Haskell][haskell] using the
[Yesod Web Framework][yesod]. It uses [bootstrap][bootstrap] as its main web
framework and [Prism][prism] as a syntax highlighter.

Getting started
---------------

You first need to install the Haskell compiler [GHC][ghc] and
[Cabal-install][cabal]. Instead of installing it individually, I
recommend installing it both from [Haskell Platform][platform] or from your
Linux distribution's package manager. Currently, I am working with GHC 7.10.1
and the package versions mentioned in cabal.config, but newer version may work
as well.

After installing the Haskell Platform, you should get the sources of this blog
software. I recommend creating a cabal sandbox for the blog
with the command `cabal sandbox init` in the main directory of the project.
After that, you can already try to install the software with `cabal install`.
This will get all dependencies and install these in your sandbox before
compiling the blog software itself.

During the first installation, which may take some time, you can start to
configure your application. Change into the config directory `cd config` and
copy the configuration samples.

```
cp settings.yml.sample settings.yml
cp postgresql.yml.sample postgresql.yml
```

Configure the settings as you prefer. Most important is to create a new
database user and the new database (replace $username with the actual username
and $dbname with the actual database name):

```
$ su
# su postgresql
# createuser $username -P
# psql
postgres=# CREATE DATABASE $dbname OWNER $username;
postgres=# \q
```

By default, postgreSQL is not configured to login via password in many 
Linux Distributions. Instead, you are able to login with the current UNIX user.
To test logging in with username and password, try the following command:  
`psql -U $username -W`  
If this does not work, you can still edit the file `data/pg_hba.conf` in the 
home-directory of the user `postgres` to accept the authentification method
`md5`.

After that, you can start Yesod as usual with the command in production mode:  
`.cabal-sandbox/bin/blog Production`

If you want to use the Yesod development server, you also need to install
the package `yesod-bin`, which is not included in the dependencies:  
`cabal install yesod-bin`  
The development server can then be started with:  
`.cabal-sandbox/bin/yesod devel`

[blog]: http://lusku.de/blog "Blog of Bastian Holst"
[haskell]: http://www.haskell.org/ "Haskell"
[yesod]: http://www.yesodweb.com/ "Yesod Web Framework"
[bootstrap]: http://getbootstrap.com/ "Bootstrap HTML, CSS and JS framework"
[prism]: http://prismjs.com/ "Prism syntax highlighter"
[ghc]: http://ghc.haskell.org/ "The Glasgow Haskell Compiler"
[cabal]: http://www.haskell.org/cabal/ "Cabal"
[platform]: http://www.haskell.org/platform/ "Haskell Platform"
