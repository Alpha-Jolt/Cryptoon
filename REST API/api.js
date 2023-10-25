const express = require('express');
const app = express();
const port = 3000;

app.use(express.json());

app.get('/api/products', (req, res) => {
    // Handle a GET request to retrieve a list of products
    res.json({ message: 'Get all products' });
});

app.get('/api/products/:id', (req, res) => {
    // Handle a GET request to retrieve a single product by ID
    const productId = req.params.id;
    res.json({ message: `Get product with ID ${productId}` });
});

app.post('/api/products', (req, res) => {
    // Handle a POST request to create a new product
    const newProduct = req.body;
    res.json({ message: 'Create a new product', data: newProduct });
});

app.put('/api/products/:id', (req, res) => {
    // Handle a PUT request to update a product by ID
    const productId = req.params.id;
    const updatedProduct = req.body;
    res.json({ message: `Update product with ID ${productId}`, data: updatedProduct });
});

app.delete('/api/products/:id', (req, res) => {
    // Handle a DELETE request to delete a product by ID
    const productId = req.params.id;
    res.json({ message: `Delete product with ID ${productId}` });
});

// Error handling middleware
app.use((err, req, res, next) => {
    console.error(err.stack);
    res.status(500).json({ message: 'Internal Server Error' });
});

//logging
app.listen(port, () => {
    console.log(`Server is running on port ${port}`);
});

//RestAPI for YOLO