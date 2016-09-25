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

casper.test.begin('Authenticate the app via Twitter', 2, function(test) {
  casper.start(hostport + '/login', function() {
    return this.fill(
      'form[action*="https://api.twitter.com/oauth/authenticate"]',
      // must use single quotes for keys with brackets
      { 'session[username_or_email]': username, 'session[password]': password }, true);
  });

  casper.waitForSelector('#filter-form', function() {
    var h1 = this.evaluate(function() {
      return document.querySelector('h1').innerHTML;
    });
    test.assertEqual(h1, 'Hi @' + username, 'Username is in the <h1>');
    test.assertExists('form[action="/block"]', "Block composer form is present");
  });

  casper.thenOpen('https://twitter.com/logout', function() {
    return this.fill('form[action*="/logout"]', {}, true);
  });

  casper.run(function() {
    test.done();
  });
});
