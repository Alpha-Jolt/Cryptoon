const express = require('express');
const bodyParser = require('body-parser');
const app = express();
const port = 3000; // Change this to your desired port number

app.use(bodyParser.urlencoded({ extended: true }));

// Serve static files (e.g., HTML, CSS, and images)
app.use(express.static('public'));

app.post('/process_signup', (req, res) => {
    const { username, password } = req.body;

    // Here, you can implement your database logic to store the user data securely.
    // You should use a secure password hashing library (e.g., bcrypt) to hash and store the password.

    // For this example, we'll just print the data.
    console.log('Received sign-up data:');
    console.log('Username:', username);
    console.log('Password:', password);

    // You can send a response indicating success or failure to the client.
    res.send('Sign-up successful'); // You should handle errors appropriately.
});

app.listen(port, () => {
    console.log(`Server is running on port ${port}`);
});
