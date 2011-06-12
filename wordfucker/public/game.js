var GameState = {
  setPuzzle: function(puzzle) {
    this.puzzleId = puzzle[0]
    this.letters = puzzle[1];
    this.children = puzzle[2];
    this.shas = $.map(this.children, function(val, i) { return val[1]; }); 
    this.found = [];
  },
  score: function () {
    return this.found.length + '/' + this.children.length;
  },
  isDone: function() { return this.found.length == this.children.length; },
  // Returns true if the guess was new
  recordGuess: function(g, sha) {
    var thing = [g.length, sha];
    if (this.shas.indexOf(sha) >= 0) {
      if (this.found.indexOf(g) >= 0) {
        return false;
      } else {
        this.found.push(g);
        localStorage[this.puzzleId] = JSON.stringify(this.found);
        return true;
      }
    }
    return false;
  }
};

var View = {
  winner: function () {
    $('#party-time').css('display', 'block');
    $('#guess_text').attr('disabled', true);
    $('#party-time button').focus();
  },
  updateScore: function (s) {
    $('#score').html(s);
  },
  resetInput: function() {
    $('.letter').removeClass('pressed');
    $('.letter').addClass('not-pressed');
    $('#guess_text').val('');
  },
  displayPuzzle: function(pid, word, children) {
    $('#fails').html('');
    $('#party-time').css('display', 'none');
    $('#guess_text').attr('disabled', false);
    var show = $.map(word, function(v, i) { 
      return '<span class="not-pressed letter value-' + v + '">' + v + '</span>';
    }).join('');
    $('#title').html(show);
    $('#holes').html('');
    $('#info').html('#' + pid);
    var words = children.slice(0);
    while (words.length > 0) {
      var chunk = words.splice(0, 5);
      var middle = $.map(chunk, function(val, i) {
        var sha = val[1];
        return ('<li class=hidden-word id=word-' + sha + '>' + hidden(val[0]) + '</li>');
      }).join('\n');
      $('#holes').append('<ul>' + middle + '</ul>');
    }
    $('#guess_text').focus();
  },
  showFail: function() {
    $('#fails').append('.');
    $('#guess_text').stop(true, true);
    $('#guess_text').effect('highlight', { "color": "#ffaaaa" }, 500);
  },
};

function checkGuess(guess, loading) {
  var g = guess.toLowerCase();
  var sha = MD5(g); 
  var word_bit = $('#word-' + sha);
  if (word_bit.length) {
    // TODO: lot of view shiz here
    word_bit[0].innerHTML = guess; // Attempt to update the text anyway.
    word_bit.removeClass('hidden-word');
    var recorded = GameState.recordGuess(g, sha);
    if (!loading) {
      if (recorded) {
        $('#guess_text').effect('highlight', {"color": "#aaffaa"}, 500);
      } else {
        $('#guess_text').effect('highlight', {"color": "#ffffaa"}, 500);
        word_bit.effect('pulsate', {"times":1}, 250);
      }
    }
    View.updateScore(GameState.score());
    if (GameState.isDone()) {
      View.winner();
    }
  } else {
    // this is bad when loading, means the puzzle has changed.
    if (!loading) {
      View.showFail();
    }
  }
}

$('#guess_text').keypress(function (e) {
  var press = String.fromCharCode(e.which).toLowerCase();
  if (GameState.letters.indexOf(press) >= 0) {
    $('.not-pressed.value-' + press + ':first').addClass('pressed');
    $('.not-pressed.value-' + press + ':first').removeClass('not-pressed');
  }
});

function countChar(word, chr) {
  return $.map(word, function (n,i) { if (n == chr) return n; else return null; });
}

// Trapping backspace only here, doesn't get thrown up as a keypress
$('#guess_text').keydown(function (e) {
  if (e.which == "8") {
    var f = $("#guess_text").val();
    if (!f.length) return;
    var ch = f[f.length -1].toLowerCase();
    if (countChar(f, ch) <= countChar(GameState.letters, ch)) {
      $('.pressed.value-' + ch + ':first').addClass('not-pressed');
      $('.pressed.value-' + ch + ':first').removeClass('pressed');
    }
  }
});

$('#guess').submit(function() {
  try {
    var guess = $('#guess_text').val().toLowerCase();
    checkGuess(guess);
    View.resetInput();
  } catch(e) {
    console.error(e);
  }
  return false;
});

$('#party-time button').click(function () {
  window.location.hash = "";
  requestPuzzle();
});

function hidden(size) { 
  var word = "";
  for (var i = 0; i < size; i++) {
    word += "*";
  }
  return word;
}

function receivePuzzle(puzzle) {
  var puzzleId = puzzle[0];
  GameState.setPuzzle(puzzle);
  View.updateScore(GameState.score());
  View.displayPuzzle(GameState.puzzleId, GameState.letters, GameState.children);
  var guesses = Data.previousGuesses(puzzleId);
  $.each(guesses, function() { checkGuess(this, true); });
  Data.recordVisit(puzzleId);
}

function requestPuzzle(id) {
  var pid = (id == undefined) ? "next" : id; 
  $.get("/puzzles/" + pid, {}, receivePuzzle, "json");
}

var Data = {
  init: function() {
    if (localStorage.puzzles == undefined) {
      localStorage.puzzles = JSON.stringify([]);
    }   
  },
  recordVisit: function(pid) {
    var pids = JSON.parse(localStorage.puzzles);
    pids.push(pid);
    localStorage.puzzles = JSON.stringify(pids);
  },
  previousGuesses: function(puzzleId) {
    var raw = localStorage[puzzleId]; 
    return raw ? JSON.parse(raw) : [];
  }
}

$(function () {
  Data.init();
  var puzzleId = (window.location.hash != "") ? window.location.hash.substring(1) : undefined;
  requestPuzzle(puzzleId);
});

