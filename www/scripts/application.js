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

(function(){
  var filterForm = document.getElementById('filter-form');
  if (!filterForm)
    return;

  var addFilterButton = document.getElementById('add-filter');
  if (!addFilterButton)
    return;

  addFilterButton.addEventListener('click', addFilter, false);

  function addFilter(_) {
    var filters = document.getElementById('filters');
    if (!filters)
      return;

    var filterCount = document.getElementsByClassName('filter').length;
    var newFilter = document.createElement('div');
    newFilter.setAttribute('class', 'filter');
    newFilter.innerHTML = makeFilterHTML(filterCount);
    filters.insertBefore(newFilter, null);
  }

  function makeFilterHTML (number) {
    return "<div class='filter'>" +
      "<label for='filter-" + number + "a'>and</label>" +
      "<select name='filter-" + number + "a' id='filter-" + number + "a'>" +
      "<option value='follows'>follows</option>" +
      "<option value='is-followed-by'>is followed by</option>" +
      "</select>" +
      "<label hidden for='filter-" + number + "b'>Twitter user</label>" +
      "<input type='text' name='filter-" + number + "b' id='filter-" + number + "b' placeholder='@username' />" +
      "</div>";
  }
})();
