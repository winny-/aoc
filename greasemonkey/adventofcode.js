// -*- mode: javascript; -*-
// jshint esversion: 6

// ==UserScript==
// @name            Advent of Code Adjustments
// @author          Winston Weinert
// @namespace       https://winny.tech/#advent-of-code
// @description     Modify the Advent of Code style.  This removes text glow,
//                  website-specific font and font size (Set your default
//                  Monospace font and size in browser settings).  It also
//                  reduces the visibility of the sponsors sidebar as to not
//                  confuse the reader.  It also adds some list vertical
//                  padding.
// @license         Unlicense
// @version	    0.1
// @include         https://adventofcode.com/*
// @released        2021-12-11
// @updated         2021-12-11
// @compatible      Greasemonkey
// ==/UserScript==

(function(){

  console.log("Running Advent of Code Adjustments...");

  // Create a stylesheet.
  function appendStylesheet(css) {

    var styletag = document.createElement('style');
    styletag.setAttribute('type', 'text/css');
    styletag.setAttribute('media', 'screen');
    styletag.appendChild(document.createTextNode(css));

    document.getElementsByTagName('head')[0].appendChild(styletag);
  }

  appendStylesheet(`
/* Your stylesheet here */
body, pre, code {
  font-size: inherit !important;
  font-family: "Monospace" !important;
}

.day-success, article em, header h1 a, header h1 span {
  font-weight: bold;
  text-shadow: none !important;
}

article li:not(:first-child) {
  padding-top: .5em;
}

#sponsor a { font-weight: bold; }
/* I guess opacity is additive, which is contrary to the CSS model? */
#sponsor .quiet { opacity: 1; }
#sponsor { opacity: .4; }
`);
})();
