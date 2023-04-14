function tryHideHypothesis(element) {
  var onoff_element = document.querySelector('.hi-onoff');
  if (Boolean(onoff_element)) {
    if (onoff_element.getAttribute('data-visible') == "no") {
      destroyHypothesis();
    }
  }
}

function reloadHypothesis() {
  var H_SERVER = 'https://hypothes.is';

  // Remove existing Hypothesis instance (if any)
  var links = [].slice.apply(document.querySelectorAll('link'));
  links.forEach(function (linkEl) {
    if (linkEl.type === 'application/annotator+html' ||
        linkEl.href.startsWith(H_SERVER)) {
      linkEl.remove();
    }
  });
  var scripts = [].slice.apply(document.querySelectorAll('script'));
  var embed_path = '';
  scripts.forEach(function (scriptEl) {
    if (scriptEl.src.startsWith(H_SERVER)) {
      scriptEl.remove();
    }
    if (scriptEl.classList.contains("hi_embed")) {
      embed_path = scriptEl.src
      scriptEl.remove();
    }
  });

  if (window.annotator) {
    // Remove event listeners for existing Hypothesis
    // instance. This works around a bug in Hypothesis <= 0.21.0
    // where window.annotator.destroy() fails to do this itself.
    var jq = require('jquery');
    var events = ['beforeAnnotationCreated',
                  'annotationCreated',
                  'annotationUpdated',
                  'annotationsLoaded',
                  'annotationsUnloaded',
                  'annotationDeleted',
                  'panelReady',
                  'setVisibleHighlights'];
    events.forEach(function (event) {
      jq(document.body).unbind(event);
    });

    // Remove the Hypothesis UI from the page
    window.annotator.destroy();
  }

  // Install Hypothesis for current URL
  var embedScriptEl = document.createElement('script');
  embedScriptEl.classList.add("hi_embed");
  embedScriptEl.src = embed_path;
  document.head.appendChild(embedScriptEl);
}

function destroyHypothesis() {
  var annotatorLink = document.querySelector(
    'link[type="application/annotator+html"]'
  );

  if (annotatorLink) {
    // Dispatch a 'destroy' event which is handled by the code in
    // annotator/main.js to remove the client.
    var destroyEvent = new Event('destroy');
    annotatorLink.dispatchEvent(destroyEvent);
  }
}

function toggleHypothesis(element, show_label, hide_label) {
  var button = element;

  if (button.getAttribute('data-visible') == "yes") {
    destroyHypothesis();
    button.innerHTML = show_label;
    button.setAttribute('data-visible', 'no');
  } else {
    reloadHypothesis();
    button.innerHTML = hide_label;
    button.setAttribute('data-visible', 'yes');
  }
}
