# wai-servlet

[![Build Status](https://circleci.com/gh/jneira/wai-servlet.svg?style=shield&circle-token=abb4f8f1c84e646a32df3143ae9ce5210eabadfa)](https://circleci.com/gh/jneira/wai-servlet)

Library to integrate [eta](http://eta-lang.org) [wai](https://github.com/yesodweb/wai) applications with the [servlet api](http://docs.oracle.com/javaee/7/api/javax/servlet/package-summary.html)

## Getting Started
* Visit the [Getting Started](http://eta-lang.org/docs/html/getting-started.html) eta instructions to build and install the library and the examples
* In [src/Network/Wai/Servlet/Examples.hs](https://github.com/jneira/wai-servlet/blob/master/src/Network/Wai/Servlet/Examples.hs) you can find some examples of wai applications and code to generate a servlet class that can be deployed in your favorite servlet container.
* There are two options to deploy wai-servlet apps: generating a war file to be deployed in a servlet container or run it directly in a embedded one; this is the way more similar to use [wai warp](https://github.com/yesodweb/wai/tree/master/warp) for haskell wai applications.

### Running an application in a embedded servlet container

* You need to install and set the [wai-servlet-jetty-adapter](https://github.com/jneira/wai-servlet-handler-jetty) package as a dependency. Currently is the unique adapter implemented for wai-servlet.
* You have to import the module `Network.Wai.Servlet.Handler.Jetty` with the `run` function and call it with the port server and your application:
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types                 (status200)
import Network.Wai
import Network.Wai.Servlet
import Network.Wai.Servlet.Handler.Jetty

appSimple :: Application
appSimple _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = run 3000 appSimple
```

### Generating a war to be deployed in a servlet container
* This option supposes some manual steps. There is plans to make etlas build automatically a war file, tracked in [this issue](https://github.com/typelead/eta/issues/265).
* The main function to generate the servlet is `Network.Wai.Servlet.makeServiceMethod` for example:
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Java
import Network.Wai
import Network.HTTP.Types                 (status200)
import Network.Wai.Servlet

appSimple :: Application
appSimple _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

servSimple :: DefaultWaiServletApplication
servSimple = makeServiceMethod appSimple

foreign export java "service" servSimple :: DefaultWaiServletApplication
```
* This code will generate a `network.wai.servlet.DefaultWaiServlet` java class that extends `javax.servlet.GenericServlet` suitable to use in a standard war file
* To generate the war you have to create the standard directory structure:
  * webApp
    * static resources (jsp,html,etc)
    * META-INF
    * WEB-INF
      * web.xml (required if you use servlets)
      * classes
      * lib
        * wai-servlet-app.jar
* The web.xml for the default wai servlet could be
```xml
<?xml version="1.0" encoding="UTF-8"?>
<web-app id="WebApp_ID" version="2.5" xmlns="http://java.sun.com/xml/ns/javaee" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd">
	<display-name>wai-servlet-test</display-name>

	<servlet>
		<servlet-name>wai-servlet</servlet-name>
		<servlet-class>network.wai.servlet.DefaultWaiServlet</servlet-class>
	</servlet>
	
	<servlet-mapping>
		<servlet-name>wai-servlet</servlet-name>
		<url-pattern>/</url-pattern>
	</servlet-mapping>
	
</web-app>
```
* You have to place the jar or jars generated by etlas in WEB-INF/lib and package the structure in a war file (using jar tool f.e.)
* With the war file you can deploy your wai application in a servlet container:
  * tomcat: https://tomcat.apache.org/tomcat-8.0-doc/deployer-howto.html
    * the easier way is to place the war file in $TOMCAT_INSTALL_DIR$/webapps
  * jetty: https://www.eclipse.org/jetty/documentation/9.4.x/configuring-deployment.html
