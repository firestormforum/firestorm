# Firestorm's Design

This is the design document for Firestorm.  It will grow over time to become an
extensive document detailing all of our design decisions and guidance.

## Mockups

We have some mockups available:

- [Mobile](https://marvelapp.com/9j51ifg)
- [Tablet](https://marvelapp.com/30jjg05)

## Planning

To see the planning work that happened, see the following documents. Focus
primarily on the design meetings, as that's where we go into more detail. The
documents themselves that are linked from each meeting are valuable, and we go
through them and have discussions in the associated videos.

- [Design Meetings](./DESIGN_MEETINGS.md)
- [UX](./UX.md)
- [Data Model](./DATA_MODEL.md)
- [OTP](./OTP.md)
- [Phoenix](./PHOENIX.md)

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
  - However we'll also implement a pretty solid jquery'd up basic interface so
    that people that do not wish to go for a Single-Page-App style client still
    have a solid foundation and can use the app in full. This will be done
    before building an Elm client.
