function highlight() {
  document.querySelectorAll('pre code').forEach((block) => {
    if (!hljs || !hljs.highlightBlock) return;
    hljs.highlightBlock(block);
  });
}

app.ports.highlightSyntax.subscribe(() => {
  setTimeout(highlight, 0);
  setTimeout(highlight, 90);
  setTimeout(highlight, 200);
  setTimeout(highlight, 1000);
});
