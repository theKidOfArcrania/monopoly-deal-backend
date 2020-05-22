{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module MonopolyDeal.Handler.SwaggerFront where

import Prelude ()
import MonopolyDeal.Import

versionString :: SwaggerVersion -> String
versionString vers = 
  map repl $ show vers
 where repl '_' = '.'
       repl 'V' = 'v'
       repl c = c

verParam :: Text
verParam = "ver"

getApiUIR :: Handler Html
getApiUIR = defaultLayout $ do
  rend <- getUrlRenderParams
  req <- getRequest
  ver <- map (maybe "Latest version" versionString) $ maybeParam verParam
  let innerUrl = rend (ViewApiR SwaggerFrontR) $ reqGetParams req
      jumpVer v = rend (ViewApiR ApiUIR) $
        [(verParam, pack $ show (v :: SwaggerVersion))]


  setTitle "Monopoly Deal API"
  addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css"
  addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
  addScriptRemote "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
  addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/js/bootstrap.min.js"
  toWidget [hamlet|
    <style>
      body, html { height: 100%; width: 100%; overflow: hidden }
    <nav class="navbar navbar-expand-lg bg-dark navbar-dark ">
      <a class="navbar-brand" href="#">
        Monopoly Deal API
      <button class="navbar-toggler" type="button" data-toggle="collapse" 
        data-target="#navbarTogglerDemo02" aria-controls="navbarTogglerDemo02"
        aria-expanded="false" aria-label="Toggle navigation">
          <span class="navbar-toggler-icon">
      <div class="collapse navbar-collapse" id="navbarTogglerDemo02">
        <ul class="navbar-nav">
          <li class="nav-item dropdown">
            <button class="btn btn-secondary dropdown-toggle" type="button" 
              id="btnApiSelection" data-toggle="dropdown" aria-haspopup="true" 
              aria-expanded="false">
                #{ver}
            <div class="dropdown-menu" aria-labelledby="btnApiSelection">
              <a class="dropdown-item" href="@{ViewApiR ApiUIR}">
                Latest version
              $forall v <- versions
                <a class="dropdown-item" href="#{jumpVer v}">
                  #{versionString v}
    <div class="d-flex flex-column" style="height: 100%">
      <iframe src="#{innerUrl}" class="border-none"
        style="flex-grow: 1; border: none;">
  |]


getSwaggerFrontR :: Handler Html
getSwaggerFrontR = defaultLayout $ do
  ver <- map (maybe latestVersion id) $ maybeParam verParam

  setTitle "Swagger API"
  addStylesheetRemote "https://unpkg.com/swagger-ui-dist@3/swagger-ui.css"
  addScriptRemote "https://unpkg.com/swagger-ui-dist@3/swagger-ui-bundle.js"
  addScriptRemote "https://unpkg.com/swagger-ui-dist@3/swagger-ui-standalone-preset.js"
  toWidget [hamlet|
    <div id="swagger-ui">
  |]
  toWidget [julius|
    const ui = SwaggerUIBundle({
      url: "@{ViewApiR $ SwaggerR ver}",
      dom_id: '#swagger-ui',
      presets: [
        SwaggerUIBundle.presets.apis,
        SwaggerUIBundle.SwaggerUIStandalonePreset
      ],
      // layout: "StandaloneLayout"
    });
  |]
