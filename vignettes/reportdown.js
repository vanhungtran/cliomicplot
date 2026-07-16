<script>
/* reportdown vignette lightbox — adapted from report_theme report_zoom.js
   Click any plot/image to open a fullscreen lightbox with:
   - Zoom in/out (buttons + mouse wheel)
   - Drag-to-pan when zoomed
   - Esc or click backdrop to close */

(function () {
  function ready(fn) {
    if (document.readyState !== "loading") { fn(); }
    else { document.addEventListener("DOMContentLoaded", fn); }
  }

  ready(function () {
    var imgs = document.querySelectorAll("img");
    if (!imgs.length) { return; }

    var ov = document.createElement("div");
    ov.className = "rd-lightbox";
    ov.innerHTML =
      '<button class="rd-lb-btn rd-lb-close" title="Close (Esc)" aria-label="Close">&times;</button>' +
      '<div class="rd-lb-controls">' +
        '<button class="rd-lb-btn rd-lb-out" title="Zoom out" aria-label="Zoom out">&minus;</button>' +
        '<button class="rd-lb-btn rd-lb-reset" title="Reset" aria-label="Reset">1:1</button>' +
        '<button class="rd-lb-btn rd-lb-in" title="Zoom in" aria-label="Zoom in">&plus;</button>' +
      '</div>' +
      '<div class="rd-lb-stage"><img class="rd-lb-img" alt=""></div>';
    document.body.appendChild(ov);

    var stage = ov.querySelector(".rd-lb-stage");
    var lbImg = ov.querySelector(".rd-lb-img");
    var scale = 1, baseScale = 1, tx = 0, ty = 0, drag = false, sx = 0, sy = 0;

    function apply() {
      lbImg.style.transform =
        "translate(" + tx + "px," + ty + "px) scale(" + scale + ")";
      lbImg.style.cursor = scale > baseScale ? "grab" : "zoom-in";
    }
    function calcBaseScale() {
      if (!lbImg.naturalWidth) { return 1; }
      var vw = window.innerWidth * 0.9;
      var vh = window.innerHeight * 0.9;
      return Math.min(vw / lbImg.naturalWidth, vh / lbImg.naturalHeight, 3);
    }
    function reset() { scale = baseScale; tx = 0; ty = 0; apply(); }
    function open(src) {
      lbImg.src = src;
      function fit() {
        baseScale = calcBaseScale();
        reset();
      }
      if (lbImg.complete && lbImg.naturalWidth) { fit(); }
      else { lbImg.onload = fit; }
      ov.classList.add("open");
      document.body.style.overflow = "hidden";
    }
    function close() {
      ov.classList.remove("open"); lbImg.src = "";
      document.body.style.overflow = "";
    }
    function zoom(f) {
      scale = Math.min(8, Math.max(baseScale * 0.5, scale * f));
      if (scale <= baseScale) { scale = baseScale; tx = 0; ty = 0; }
      apply();
    }

    // Attach click handlers to all images
    Array.prototype.forEach.call(imgs, function (im) {
      if (im.classList.contains("rd-no-zoom")) { return; }
      im.classList.add("rd-zoomable");
      im.addEventListener("click", function () {
        open(im.currentSrc || im.src);
      });
    });

    // Button handlers
    ov.querySelector(".rd-lb-close").addEventListener("click", close);
    ov.querySelector(".rd-lb-in").addEventListener("click", function (e) {
      e.stopPropagation(); zoom(1.3);
    });
    ov.querySelector(".rd-lb-out").addEventListener("click", function (e) {
      e.stopPropagation(); zoom(1 / 1.3);
    });
    ov.querySelector(".rd-lb-reset").addEventListener("click", function (e) {
      e.stopPropagation(); reset();
    });

    // Click backdrop to close
    ov.addEventListener("click", function (e) {
      if (e.target === ov || e.target === stage) { close(); }
    });

    // Escape key to close
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && ov.classList.contains("open")) { close(); }
    });

    // Mouse wheel zoom
    stage.addEventListener("wheel", function (e) {
      if (!ov.classList.contains("open")) { return; }
      e.preventDefault();
      zoom(e.deltaY < 0 ? 1.15 : 1 / 1.15);
    }, { passive: false });

    // Drag to pan when zoomed in
    lbImg.addEventListener("mousedown", function (e) {
      if (scale <= baseScale) { return; }
      drag = true; sx = e.clientX - tx; sy = e.clientY - ty;
      lbImg.style.cursor = "grabbing"; e.preventDefault();
    });
    window.addEventListener("mousemove", function (e) {
      if (!drag) { return; }
      tx = e.clientX - sx; ty = e.clientY - sy; apply();
    });
    window.addEventListener("mouseup", function () {
      drag = false; if (scale > 1) { lbImg.style.cursor = "grab"; }
    });
  });
})();
</script>
