var slashes = require( ".." ),
    assert = require( "assert" ),
    http = require( "http" );

var append = slashes( true );

describe( "connect-slashes", function() {

    //
    it( "shouldn't redirect slashes for POST requests", function( done ) {
        append( { method: "POST", url: "/foo" }, {
            writeHead: function() {
                assert( false ); // attempt to modify the response
            }
        }, done );
    });

    //
    it( "shouldn't redirect slashes for PUT requests", function( done ) {
        append( { method: "PUT", url: "/foo" }, {
            writeHead: function() {
                assert( false ); // attempt to modify the response
            }
        }, done );
    });

    //
    it( "should append slashes for GET requests", function( done ) {
        append( { method: "GET", url: "/foo" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should append slashes for GET requests using originalUrl", function( done ) {
        append( { method: "GET", originalUrl: "/foo" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should remove slashes", function( done ) {
        slashes( false )( { method: "GET", url: "/foo/" }, {
            writeHead: function( status, headers ) {
                assert( "/foo" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should remove slashes when using originalUrl", function( done ) {
        slashes( false )( { method: "GET", originalUrl: "/foo/" }, {
            writeHead: function( status, headers ) {
                assert( "/foo" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should move permanenetly (301)", function( done ) {
        append( { method: "GET", url: "/foo" }, {
            writeHead: function( status, headers ) {
                assert( status == 301 );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should control the redirect status code", function( done ) {
        slashes( true, { code: 305 } )( { method: "GET", url: "/foo" }, {
            writeHead: function( status, headers ) {
                assert( status == 305 );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        } );
    });

    //
    it( "should forward GET arguments", function( done ) {
        append( { method: "GET", url: "/foo?hello=world&foo=bar" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/?hello=world&foo=bar" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        })
    });

    //
    it( "should forward (weird) GET arguments", function( done ) {
        append( { method: "GET", url: "/foo&hello=world&foo=bar" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/?hello=world&foo=bar" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        })
    });

    //
    it( "should clean up double-slashes", function( done ) { // fixes #2
        append( { method: "GET", url: "//foo" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        })
    });

    //
    it( "should prepend the base_path argument", function( done ) {
        slashes( true, { base: "/foo/" } )( { method: "GET", url: "/bar/world" }, {
            writeHead: function( status, headers ) {
                assert( "/foo/bar/world/" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        });
    });

    //
    it( "should prepend a first slash", function( done ) {
        append( { method: "GET", url: "bar/world" }, {
            writeHead: function( status, headers ) {
                assert( "/bar/world/" == headers.Location );
            },
            end: done
        }, function() {
            assert( false ); // no redirect took place
        });
    });

    it( "should set headers", function( done ) {
        slashes( true, { headers: { "Cache-Control": "public" } } )( { method: "GET", url: "/foo" }, {
            writeHead: function( status, headers ) {
                assert( "public" == headers["Cache-Control"] );
                assert( "/foo/" == headers.Location );
            },
            end: done

        }, function() {
            assert( false ); // no redirect took place
        } );
    });

} );
