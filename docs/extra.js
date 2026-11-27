/* ==========================================================================
   cliomicplot: Universal Image Zoom & Lightbox
   Enables smooth click-to-zoom and interactive pan/zoom across all .html pages.
   ========================================================================== */

(function () {
  'use strict';

  function initLightbox() {
    if (document.getElementById('cli-lightbox-overlay')) return;

    // Create lightbox DOM elements
    var overlay = document.createElement('div');
    overlay.id = 'cli-lightbox-overlay';
    overlay.className = 'cli-lightbox';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', 'Image preview zoom');

    overlay.innerHTML =
      '<button class="cli-lb-btn cli-lb-close" title="Close (Esc)" aria-label="Close">&times;</button>' +
      '<div class="cli-lb-controls">' +
        '<button class="cli-lb-btn cli-lb-zoom-out" title="Zoom out (-)" aria-label="Zoom out">&minus;</button>' +
        '<button class="cli-lb-btn cli-lb-reset" title="Fit / Reset (0)" aria-label="Reset zoom">Fit</button>' +
        '<button class="cli-lb-btn cli-lb-zoom-in" title="Zoom in (+)" aria-label="Zoom in">&plus;</button>' +
      '</div>' +
      '<div class="cli-lb-stage">' +
        '<img class="cli-lb-img" alt="Zoomed view" draggable="false">' +
      '</div>' +
      '<div class="cli-lb-caption"></div>';

    document.body.appendChild(overlay);

    var stage = overlay.querySelector('.cli-lb-stage');
    var lbImg = overlay.querySelector('.cli-lb-img');
    var captionEl = overlay.querySelector('.cli-lb-caption');
    var closeBtn = overlay.querySelector('.cli-lb-close');
    var zoomInBtn = overlay.querySelector('.cli-lb-zoom-in');
    var zoomOutBtn = overlay.querySelector('.cli-lb-zoom-out');
    var resetBtn = overlay.querySelector('.cli-lb-reset');

    var scale = 1;
    var baseScale = 1;
    var tx = 0;
    var ty = 0;
    var isDragging = false;
    var startX = 0;
    var startY = 0;
    var initialDistance = 0;
    var initialScale = 1;

    function applyTransform(smooth) {
      lbImg.style.transition = smooth ? 'transform 0.22s cubic-bezier(0.16, 1, 0.3, 1)' : 'none';
      lbImg.style.transform = 'translate(' + tx + 'px, ' + ty + 'px) scale(' + scale + ')';
      lbImg.style.cursor = scale > baseScale ? (isDragging ? 'grabbing' : 'grab') : 'zoom-in';
      if (resetBtn) {
        var pct = Math.round((scale / baseScale) * 100);
        resetBtn.textContent = pct === 100 ? 'Fit' : pct + '%';
      }
    }

    function calcBaseScale() {
      if (!lbImg.naturalWidth) return 1;
      var vw = window.innerWidth * 0.92;
      var vh = window.innerHeight * 0.88;
      var fitRatio = Math.min(vw / lbImg.naturalWidth, vh / lbImg.naturalHeight);
      return Math.min(fitRatio, 1.5);
    }

    function resetView(smooth) {
      baseScale = calcBaseScale();
      scale = baseScale;
      tx = 0;
      ty = 0;
      applyTransform(smooth !== false);
    }

    function openLightbox(src, altText) {
      lbImg.src = src;
      if (altText && altText.trim().length > 0) {
        captionEl.textContent = altText;
        captionEl.style.display = 'block';
      } else {
        captionEl.textContent = '';
        captionEl.style.display = 'none';
      }

      function onLoaded() {
        resetView(false);
        overlay.classList.add('active');
        document.body.classList.add('cli-lb-open');
      }

      if (lbImg.complete && lbImg.naturalWidth) {
        onLoaded();
      } else {
        lbImg.onload = onLoaded;
      }
    }

    function closeLightbox() {
      overlay.classList.remove('active');
      document.body.classList.remove('cli-lb-open');
      setTimeout(function () {
        if (!overlay.classList.contains('active')) {
          lbImg.src = '';
          captionEl.textContent = '';
        }
      }, 250);
    }

    function zoomBy(factor) {
      var prevScale = scale;
      scale = Math.min(baseScale * 8, Math.max(baseScale * 0.4, scale * factor));
      if (scale <= baseScale * 1.02) {
        scale = baseScale;
        tx = 0;
        ty = 0;
      } else {
        tx = tx * (scale / prevScale);
        ty = ty * (scale / prevScale);
      }
      applyTransform(true);
    }

    function bindImages() {
      var allImgs = document.querySelectorAll('img');
      for (var i = 0; i < allImgs.length; i++) {
        var img = allImgs[i];

        if (img.classList.contains('logo') ||
            img.classList.contains('cli-lb-img') ||
            img.classList.contains('rd-no-zoom') ||
            img.classList.contains('no-zoom') ||
            img.closest('.navbar') ||
            img.closest('footer') ||
            img.closest('.pkgdown-footer') ||
            img.getAttribute('width') === '16' ||
            img.getAttribute('height') === '16') {
          continue;
        }

        if (!img.classList.contains('cli-zoomable')) {
          img.classList.add('cli-zoomable');

          // Prevent link navigation if image is wrapped in <a>
          var parentLink = img.closest('a');
          if (parentLink) {
            parentLink.addEventListener('click', function (e) {
              var href = this.getAttribute('href');
              if (href && (/\.(png|jpe?g|svg|webp|gif)($|\?)/i.test(href) || href === '#' || href.startsWith('#'))) {
                e.preventDefault();
              }
            });
          }

          (function (targetImg) {
            targetImg.addEventListener('click', function (e) {
              e.preventDefault();
              e.stopPropagation();
              var fullSrc = targetImg.getAttribute('data-src') || targetImg.currentSrc || targetImg.src;
              var alt = targetImg.getAttribute('alt') || targetImg.getAttribute('title') || '';
              openLightbox(fullSrc, alt);
            });
          })(img);
        }
      }
    }

    bindImages();

    if (window.MutationObserver) {
      var observer = new MutationObserver(function () {
        bindImages();
      });
      observer.observe(document.body, { childList: true, subtree: true });
    }

    closeBtn.addEventListener('click', closeLightbox);
    zoomInBtn.addEventListener('click', function (e) { e.stopPropagation(); zoomBy(1.35); });
    zoomOutBtn.addEventListener('click', function (e) { e.stopPropagation(); zoomBy(1 / 1.35); });
    resetBtn.addEventListener('click', function (e) { e.stopPropagation(); resetView(true); });

    stage.addEventListener('dblclick', function (e) {
      if (e.target === closeBtn || e.target.closest('.cli-lb-controls')) return;
      if (scale > baseScale * 1.2) {
        resetView(true);
      } else {
        scale = baseScale * 2.2;
        applyTransform(true);
      }
    });

    overlay.addEventListener('click', function (e) {
      if (e.target === overlay || e.target === stage) {
        closeLightbox();
      }
    });

    document.addEventListener('keydown', function (e) {
      if (!overlay.classList.contains('active')) return;
      if (e.key === 'Escape') {
        closeLightbox();
      } else if (e.key === '+' || e.key === '=') {
        zoomBy(1.25);
      } else if (e.key === '-' || e.key === '_') {
        zoomBy(1 / 1.25);
      } else if (e.key === '0') {
        resetView(true);
      }
    });

    stage.addEventListener('wheel', function (e) {
      if (!overlay.classList.contains('active')) return;
      e.preventDefault();
      var factor = e.deltaY < 0 ? 1.18 : 1 / 1.18;
      zoomBy(factor);
    }, { passive: false });

    // Drag to pan
    lbImg.addEventListener('mousedown', function (e) {
      if (scale <= baseScale) return;
      isDragging = true;
      startX = e.clientX - tx;
      startY = e.clientY - ty;
      applyTransform(false);
      e.preventDefault();
    });

    window.addEventListener('mousemove', function (e) {
      if (!isDragging) return;
      tx = e.clientX - startX;
      ty = e.clientY - startY;
      applyTransform(false);
    });

    window.addEventListener('mouseup', function () {
      if (isDragging) {
        isDragging = false;
        applyTransform(false);
      }
    });

    // Touch support (pinch-to-zoom & pan)
    stage.addEventListener('touchstart', function (e) {
      if (e.touches.length === 1) {
        if (scale > baseScale) {
          isDragging = true;
          startX = e.touches[0].clientX - tx;
          startY = e.touches[0].clientY - ty;
        }
      } else if (e.touches.length === 2) {
        isDragging = false;
        initialDistance = Math.hypot(
          e.touches[0].clientX - e.touches[1].clientX,
          e.touches[0].clientY - e.touches[1].clientY
        );
        initialScale = scale;
      }
    }, { passive: true });

    stage.addEventListener('touchmove', function (e) {
      if (e.touches.length === 1 && isDragging) {
        tx = e.touches[0].clientX - startX;
        ty = e.touches[0].clientY - startY;
        applyTransform(false);
      } else if (e.touches.length === 2 && initialDistance > 0) {
        var dist = Math.hypot(
          e.touches[0].clientX - e.touches[1].clientX,
          e.touches[0].clientY - e.touches[1].clientY
        );
        scale = Math.min(baseScale * 8, Math.max(baseScale * 0.4, initialScale * (dist / initialDistance)));
        applyTransform(false);
      }
    }, { passive: true });

    stage.addEventListener('touchend', function (e) {
      if (e.touches.length < 2) initialDistance = 0;
      if (e.touches.length === 0) isDragging = false;
    }, { passive: true });

    window.addEventListener('resize', function () {
      if (overlay.classList.contains('active')) {
        baseScale = calcBaseScale();
        if (scale <= baseScale * 1.1) {
          scale = baseScale;
          tx = 0;
          ty = 0;
        }
        applyTransform(false);
      }
    });
  }

  /* ==========================================================================
     cliomicplot: Code Folding for Articles and Vignettes
     Wraps code chunks in collapsible <details> with "Show R code" toggle.
     Adds a global "Show All Code / Hide All Code" toggle to article headers.
     ========================================================================== */
  function initCodeFolding() {
    var sourceBlocks = document.querySelectorAll('.template-article main div.sourceCode, .template-article main pre.sourceCode, .template-article main pre.downlit');
    if (!sourceBlocks.length) {
      sourceBlocks = document.querySelectorAll('main div.sourceCode, main pre.sourceCode, main pre.downlit');
    }
    if (!sourceBlocks.length) return;

    var detailsList = [];

    sourceBlocks.forEach(function (block) {
      if (block.closest('details') || block.dataset.cliCodeFolded) return;
      block.dataset.cliCodeFolded = 'true';

      var details = document.createElement('details');
      details.className = 'cli-code-fold';
      
      var summary = document.createElement('summary');
      summary.className = 'cli-code-summary';
      summary.innerHTML = '<span class="cli-code-icon">▶</span> <span class="cli-code-label">Show R code</span>';

      block.parentNode.insertBefore(details, block);
      details.appendChild(summary);
      details.appendChild(block);

      details.addEventListener('toggle', function () {
        var icon = summary.querySelector('.cli-code-icon');
        var label = summary.querySelector('.cli-code-label');
        if (details.open) {
          if (icon) icon.textContent = '▼';
          if (label) label.textContent = 'Hide R code';
        } else {
          if (icon) icon.textContent = '▶';
          if (label) label.textContent = 'Show R code';
        }
      });

      detailsList.push(details);
    });

    if (detailsList.length > 0) {
      var headerTarget = document.querySelector('main header, main h1');
      if (headerTarget && !document.getElementById('cli-toggle-all-code-btn')) {
        var toolbar = document.createElement('div');
        toolbar.className = 'cli-code-toolbar mb-3 text-end';
        toolbar.innerHTML = '<button id="cli-toggle-all-code-btn" class="btn btn-sm btn-outline-secondary" type="button"><i class="bi bi-code-slash me-1"></i> Show All Code</button>';
        
        var btn = toolbar.querySelector('#cli-toggle-all-code-btn');
        var allExpanded = false;
        btn.addEventListener('click', function () {
          allExpanded = !allExpanded;
          detailsList.forEach(function (d) { d.open = allExpanded; });
          btn.innerHTML = allExpanded ? '<i class="bi bi-code-slash me-1"></i> Hide All Code' : '<i class="bi bi-code-slash me-1"></i> Show All Code';
        });

        headerTarget.parentNode.insertBefore(toolbar, headerTarget.nextSibling);
      }
    }
  }

  function init() {
    initLightbox();
    initCodeFolding();
  }

  if (document.readyState !== 'loading') {
    init();
  } else {
    document.addEventListener('DOMContentLoaded', init);
  }
})();
