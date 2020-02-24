app.ports.shorten.subscribe(function (u) {
  const url = u.indexOf('//') >= 0 ? u : `https://${u}`;
  shorten(url).then(shortenedUrl => {
    app.ports.getShortenedUrl.send(shortenedUrl);
  });
});
app.ports.copyToClipboard.subscribe(function (url) {
  const t = document.createElement('textarea');
  document.body.appendChild(t);
  t.value = url;
  t.select();
  document.execCommand('copy');
  document.body.removeChild(t);
});


function makeid(length) {
  let result = '';
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  const charactersLength = characters.length;
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

async function checkHash(hash) {
  const querySnapshot = await db.collection('urls')
    .doc(hash)
    .get();
  return !querySnapshot.exists;
}

async function shorten(url) {
  const hash = makeid(5);
  if (!await checkHash(hash)) {
    return await shorten(url);
  }
  await db.collection('urls').doc(hash).set({
    url
  });
  return hash;
}