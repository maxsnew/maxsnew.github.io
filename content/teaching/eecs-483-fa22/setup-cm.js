jQuery(document).ready(function($) {
  function extractMarks(text) {
    var lines = text.split(/\r\n?|\n/);
    var marks = {byLine: [], byNum: Object.create(null)};
    var count = 0;
    for (var lineNo = 0; lineNo < lines.length; lineNo++) {
      marks.byLine[lineNo] = [];
      var idx = undefined;
      while ((idx = lines[lineNo].search(/~hl:\d+:[se]~/)) >= 0) {
        var m = lines[lineNo].match(/~hl:(\d+):([se])~/);
        lines[lineNo] = lines[lineNo].replace(m[0], "");
        if (m[2] === "s") {
          count++;
          marks.byNum[m[1]] = {startLine: lineNo, startCol: idx, number: m[1]};
          if (marks.byLine[lineNo][idx] === undefined)
            marks.byLine[lineNo][idx] = {open: [], close: []};
          marks.byLine[lineNo][idx].open.push(marks.byNum[m[1]]);
        }
        else if (marks.byNum[m[1]] !== undefined) {
          marks.byNum[m[1]].endCol = idx;
          marks.byNum[m[1]].endLine = lineNo;
          if (marks.byLine[lineNo][idx] === undefined)
            marks.byLine[lineNo][idx] = {open: [], close: []};
          marks.byLine[lineNo][idx].close.unshift(marks.byNum[m[1]]);
        } else {
          console.error("No information found for mark [" + m.join(", ") + "]")
        }
      }
    }
    return {count: count, text: lines.join("\n"), marks: marks};
  }

  function applyMarks(code, allMarks) {
    var curCol = 0;
    var curLine = 0;
    var openMarks = " ";
    function updateMarks(marks) {
      if (marks) {
        marks.close.forEach(function(m) { openMarks = openMarks.replace("hilite-" + m.number + " ", ""); });
        marks.open.forEach(function(m) {
          if (openMarks.search("hilite-" + m.number) == -1) { openMarks = "hilite-" + m.number + " " + openMarks; } });
      }
    }
    function wrapMarks(node) {
      openMarks.trim().split(" ").forEach(function(m) {
        node = $("<span>").addClass(m).addClass("hilite").append(node);
      });
      return node;
    }
    for (var i = 0; i < code.childNodes.length; i++) {
      // NOTE: This loop may mutate the childNodes and split nodes into pieces,
      // but this will only ever increase the childNodes count and so
      // won't skip any kids
      var marks = allMarks.byLine[curLine] || [];
      if (curCol === 0) {
        updateMarks(marks[0]);
      }
      var kid = code.childNodes[i];
      if (kid.nodeType === 3 && kid.textContent === "\n") {
        curCol = 0;
        curLine++;
      } else {
        var newEnd = curCol + kid.textContent.length;
        var c = curCol + 1;
        for (; c < newEnd; c++) {
          if (marks[c] !== undefined && (marks[c].open.length > 0 || marks[c].close.length > 0)) {
            var newKid1 = wrapMarks($("<span>").attr("class", $(kid).attr("class"))
                                               .text($(kid).text().substring(0, c - curCol)));
            updateMarks(marks[c]);
            var newKid2 = wrapMarks($("<span>").attr("class", $(kid).attr("class"))
                                               .text($(kid).text().substring(c - curCol)));
            $(kid).replaceWith(newKid1);
            newKid1.after(newKid2);
            curCol = c;
            break;
          }
        }
        if (c == newEnd) {
          if (openMarks !== " ") {
            code.replaceChild(wrapMarks($("<span>").text($(kid).text())
                                                   .attr("class", $(kid).attr("class")))[0], kid);
          }
          curCol = newEnd;
        }
        updateMarks(marks[curCol]);
      }
    }
  }

  $(function(){
    $("code.sourceCode").each(function(_, code) {
      if ($(code).data("lang")) {
        var markedText = extractMarks($(code).text());
        CodeMirror.runMode(markedText.text, $(code).data("lang"), code);
        if (markedText.count > 0) applyMarks(code, markedText.marks);
        $(code).addClass("cm-s-default");
      }
    });
  });
});


