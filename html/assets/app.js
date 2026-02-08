// app.js
function renderFiles(filesByTag, region) {
  // IMPORTANT: query the main page DOM, not the SVG document
  const pageDoc = window.document;

  const container = pageDoc.getElementById("selector_placeholder");
  const chartContainer = pageDoc.getElementById("chart_placeholder");

  if (!container) {
    console.error('Missing <div id="selector_placeholder"> in index.html');
    return;
  }
  if (!chartContainer) {
    console.error('Missing <div id="chart_placeholder"> in index.html');
    return;
  }

  container.innerHTML = "";

  const title = pageDoc.createElement("h3");
  title.textContent = region;
  container.appendChild(title);

  const wrapper = pageDoc.createElement("div");
  container.appendChild(wrapper);

  const tags = Object.keys(filesByTag || {}).sort((a, b) => a.localeCompare(b, "pl"));

  if (tags.length === 0) {
    const p = pageDoc.createElement("p");
    p.textContent = "No files for this region.";
    wrapper.appendChild(p);
    return;
  }

  for (const dataTag of tags) {
    const entries = filesByTag[dataTag] || {};
    const names = Object.keys(entries).sort((a, b) => a.localeCompare(b, "pl"));
    if (names.length === 0) continue;

    const details = pageDoc.createElement("details");
    details.open = false;

    const summary = pageDoc.createElement("summary");
    summary.textContent = `${dataTag} (${names.length})`;
    summary.style.fontWeight = "bold";
    summary.style.cursor = "pointer";

    const ul = pageDoc.createElement("ul");

    for (const displayName of names) {
      const filePath = entries[displayName];
      const src = filePath.startsWith("/") ? filePath : `/${filePath}`;

      const li = pageDoc.createElement("li");

      const link = pageDoc.createElement("a");
      link.href = "#";
      link.textContent = displayName;
      link.style.cursor = "pointer";

      link.addEventListener("click", (e) => {
        e.preventDefault();

        // optional: mark active link
        container.querySelectorAll("a").forEach(a => a.classList.remove("active-link"));
        link.classList.add("active-link");

        // load iframe
        chartContainer.innerHTML = "";

        const iframe = pageDoc.createElement("iframe");
        iframe.src = src;
        iframe.style.width = "100%";
        iframe.style.height = "700px";
        iframe.style.border = "0";

        chartContainer.appendChild(iframe);
      });

      li.appendChild(link);
      ul.appendChild(li);
    }

    details.appendChild(summary);
    details.appendChild(ul);
    wrapper.appendChild(details);
  }
}


(function () {
    document.addEventListener("DOMContentLoaded", function () {
        const obj = document.getElementById("svgObj");
        if (!obj) return;

        const manifest = fetch("/html/generated/manifest.json").then(r => r.json());
        const container = document.getElementById("selector_placeholder");
        obj.addEventListener("load", function () {
            const svgDoc = obj.contentDocument;
            if (!svgDoc) return;

            // inject CSS into the SVG
            const style = svgDoc.createElementNS("http://www.w3.org/2000/svg", "style");
            style.textContent = `
                .region-highlight {
                fill: #ffda03 !important;
                stroke: #000;
                stroke-width: 2;
                }
            `;
            svgDoc.documentElement.appendChild(style);

            svgDoc.querySelectorAll("path").forEach((path) => {
                path.style.cursor = "pointer";

                path.addEventListener("click", async () => {
                    // remove highlight from all paths
                    svgDoc.querySelectorAll(".region-highlight")
                        .forEach(p => p.classList.remove("region-highlight"));

                    // highlight clicked one
                    path.classList.add("region-highlight");

                    // ---- your existing logic below ----
                    const id = path.id;
                    if (!id) return;

                    const filesByTag = (await manifest)[id] || {};
                    renderFiles(filesByTag, id);
                });
            });
        });


    });
})();
