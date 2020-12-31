CREATE TABLE IF NOT EXISTS 'recipes' (
    id INTEGER PRIMARY KEY,
    title TEXT,
    ingredients TEXT,
    servings INTEGER,
    cooking_time INTEGER,
    waiting_time INTEGER,
    directions TEXT,
    slug TEXT, comments TEXT);
CREATE TABLE IF NOT EXISTS 'tags' (
    id INTEGER PRIMARY KEY,
    tag TEXT UNIQUE);
CREATE TABLE IF NOT EXISTS 'recipes_tags' (
    recipe_id INTEGER,
    tag_id INTEGER);
