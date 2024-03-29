CommonTools v2.10.7
===================
*Library used in other projects*

* Copyright 2010-21 Vincent Labatut

CommonTools is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/CommonTools
* Contact: vincent.labatut@univ-avignon.fr

**Just stored here for convenience, not meant to be used outside of the team, not much documentation**

-----------------------------------------------------------------------

## Description
This Eclipse project contains Java classes implementing general features, which are likely to be useful in other applications. 


## Use
This source code is meant to be used as utility in larger Java projects, by simply including the appropriate packages. 


## Dependencies
Here are the dependencies for CommonTools:
* [Jsoup 1.8.2](https://jsoup.org/)
  * `web` package: HTML-related features.
* [Jdom 2.0.6](http://www.jdom.org/)
  * `keys` package: access to XML files.
  * `xml` package: XML-related features.
* [Apache Commons Text 1.7](https://commons.apache.org/proper/commons-text/)
  * `string` package: string comparison.
* [Apache HttpCore 4.4.11](https://hc.apache.org/httpcomponents-core-ga/) and [Apache HttpClient 4.5.9](https://hc.apache.org/httpcomponents-client-4.5.x)
  * `web` package: HTTP-related features.
* [Apache Commons Logging 1.2](http://commons.apache.org/proper/commons-logging/)
  * Required by other Apache libraries.


## Todo
* Debug web reader
* Include graph package from BiblioProcess, move GraphmlTools to a separate subpackage
* Use the new LocalDate class instead of Date
* Check the more recent nio methods

