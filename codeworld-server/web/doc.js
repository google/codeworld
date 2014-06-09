(function() {
  var hash = window.location.hash.slice(1);
  var request = new XMLHttpRequest();
  request.open('GET', hash, true);
  request.onreadystatechange = function() {
    if (request.readyState == 4) {
      var text = request.responseText;
      var converter = new Markdown.Converter();
      var html = converter.makeHtml(text);
      document.body.innerHTML = html;

      var pres = document.getElementsByTagName('pre');
      for (var i = 0; i < pres.length; ++i) {
        (function() {
          var pre = pres[i];
          var text = pre.textContent;
          pre.innerHTML = '';
          CodeMirror.runMode(text, 'haskell', pre);
          pre.classList.add('cm-s-default');

          if (text.indexOf('main ') != -1) {
            pre.classList.add('clickable');
            pre.onclick = function() {
              if (parent && parent.setCode) {
                parent.setCode(text);
              }
            }
          }
        })();
      }
    }
  };
  request.send(null);
})();
