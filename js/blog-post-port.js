function highlight() {
  document.querySelectorAll("pre code").forEach((block) => {
    if (!hljs || !hljs.highlightBlock) return;
    hljs.highlightBlock(block);
  });

  renderMathInElement(document.body, {
    delimiters: [
      { left: "$$", right: "$$", display: true },
      { left: "$", right: "$", display: false },
      { left: "\\[", right: "\\]", display: true },
      { left: "\\(", right: "\\)", display: false },
    ],
  });
}

app.ports.highlightSyntax.subscribe(() => {
  setTimeout(highlight, 0);
  setTimeout(highlight, 90);
  setTimeout(highlight, 200);
  setTimeout(highlight, 1000);
});

app.ports.highlightSyntaxList.subscribe(() => {
  setTimeout(highlight, 0);
  setTimeout(highlight, 90);
  setTimeout(highlight, 200);
  setTimeout(highlight, 1000);
});

app.ports.scrollTo.subscribe((fragment) => {
  try {
    window.scrollTo(0, document.getElementById(fragment).offsetTop);
    setTimeout(() => {
      window.scrollTo(0, document.getElementById(fragment).offsetTop);
    }, 50);
    setTimeout(() => {
      window.scrollTo(0, document.getElementById(fragment).offsetTop);
    }, 100);
  } catch {}
});
