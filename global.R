### ---------------------------------------------------------------------------
### --- Wiktionary Cognate Dashboard
### --- Script: global.R, v. Beta 0.1
### --- WMDE 2018.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### ---
### --- Wiktionary: Cognate Dashboard Update is free software: 
### --- you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- Wiktionary: Cognate Dashboard Update is distributed in the 
### --- hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with Wiktionary: Cognate Dashboard Update. 
### --- If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 600000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"