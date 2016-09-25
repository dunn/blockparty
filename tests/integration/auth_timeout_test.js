// Copyright 2016 Alex Dunn <dunn.alex@gmail.com>

// This file is part of blockparty.

// blockparty is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License,
// or (at your option) any later version.

// blockparty is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with blockparty.  If not, see <http://www.gnu.org/licenses/>.

// casper.options.waitTimeout = 10000;

var system = require('system');
var username = system.env.BP_TEST_USER;
var password = system.env.BP_TEST_PASS;
var hostport = 'http://localhost:3000';

casper.test.begin('Authenticate the app via Twitter', 1, function(test) {
  casper.start(hostport + '/login', function() {
    return true
  });

  casper.wait(60000, function() {
    return this.fill(
      'form[action*="https://api.twitter.com/oauth/authenticate"]',
      // must use single quotes for keys with brackets
      { 'session[username_or_email]': username, 'session[password]': password }, true);
  });

  casper.wait(5000, function() {
    var errMessage = this.evaluate(function() {
      return document.querySelector('p').innerHTML;
    });
    test.assertEquals(errMessage.indexOf('Took too long to authenticate with Twitter'), 0);
  });

  casper.run(function() {
    test.done();
  });
});
