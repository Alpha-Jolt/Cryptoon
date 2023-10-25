const express = require('express');
const fs = require('fs');
const range = require('range-parser');
const app = express();
const port = 3000;

app.use(express.static(__dirname)); // Serve static files in the current directory

app.get('/video/:videoName', (req, res) => {
  const videoPath = `videos/${req.params.videoName}`;
  const stat = fs.statSync(videoPath);
  const fileSize = stat.size;

  const rangeRequest = range(fileSize, req.headers.range, { combine: true });
  if (rangeRequest === -1) {
    res.status(416).send('Requested range not satisfiable');
    return;
  } else if (rangeRequest === -2) {
    res.status(200).set({
      'Content-Type': 'video/mkv',
      'Content-Length': fileSize,
      'Accept-Ranges': 'bytes',
    });
    fs.createReadStream(videoPath).pipe(res);
  } else {
    const { start, end } = rangeRequest[0];
    const chunkSize = end - start + 1;
    res.status(206).set({
      'Content-Range': `bytes ${start}-${end}/${fileSize}`,
      'Accept-Ranges': 'bytes',
      'Content-Length': chunkSize,
      'Content-Type': 'video/mkv',
    });
    fs.createReadStream(videoPath, { start, end }).pipe(res);
  }
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});