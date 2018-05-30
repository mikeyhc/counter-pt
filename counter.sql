CREATE TABLE meal_names (
        name STRING
);

INSERT INTO meal_names VALUES('breakfast');
INSERT INTO meal_names VALUES('morning snacks');
INSERT INTO meal_names VALUES('lunch');
INSERT INTO meal_names VALUES('afternoon snacks');
INSERT INTO meal_names VALUES('breakfast');

CREATE TABLE products (
        name STRING PRIMARY KEY,
        -- energy in kJ
        energy NUMBER,
        fat NUMBER,
        carbohydrates NUMBER,
        fiber NUMBER,
        protein NUMBER,
        salt NUMBER,
        liquid BOOLEAN
);

CREATE TABLE meals (
        name STRING REFERENCES meal_names(name),
        date DATE,

        PRIMARY KEY (name, date)
);

CREATE TABLE meal_items (
        meal_name STRING,
        date DATE,
        product STRING REFERENCES product(name),
        -- qty in g or ml
        quantity NUMBER,

        FOREIGN KEY (meal_name, date) REFERENCES meal(name, date)
);
