# Firestorm's Design

This is the design document for Firestorm.  It will grow over time to become an
extensive document detailing all of our design decisions and guidance.

## Style Guide

We will have an official style guide linked here, eventually

## Resources

Here are resources that we find relevant.

### [Resilient Web Design](https://resilientwebdesign.com/)

TODO: We should identify all areas of this document that are explicit guidance
for our application.

[This is an excellent document](https://resilientwebdesign.com/) that lays out a
fantastic approach to building web applications.  It's a very well-written
outline that I find very compelling and that matches my personal beliefs
regarding how we should build things.

#### Progressive Enhancement

[Chapter 6: Steps](https://resilientwebdesign.com/chapter6/) is a great
discussion of the layers that make up a resilient web application.  To apply it
to our forum software:

- We should build a basic HTML version of the site that is accessibly with no
  CSS and no JavaScript.
- We should add a stylesheet that makes it look nice and makes it easier to
  read.
- We should add a JavaScript client that is available for those people that are
  running JavaScript.
  - My extensive experience with both JavaScript and
    [Elm](http://www.elm-lang.org) leads me to 100% certainty that the rich
    client should be written using Elm.
