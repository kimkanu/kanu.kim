function highlight() {
  document.querySelectorAll('pre code').forEach((block) => {
    hljs.highlightBlock(block);
    hljs.lineNumbersBlock(block);
  });
}

app.ports.highlightSyntax.subscribe(() => {
  setTimeout(highlight, 0);
  setTimeout(highlight, 90);
  setTimeout(highlight, 200);
  setTimeout(highlight, 1000);
});
