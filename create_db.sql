CREATE TABLE blog_posts (
    "id" SERIAL PRIMARY KEY,
    "title" TEXT NOT NULL,
    "posted_at" TIMESTAMPTZ NOT NULL DEFAULT now(),
    "body" TEXT NOT NULL,
    "slug" TEXT NOT NULL
);

CREATE INDEX ON blog_posts(slug);

CREATE TABLE comment_posters (
    "id" SERIAL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "hash" TEXT NOT NULL
);

CREATE TABLE comments (
    "id" SERIAL PRIMARY KEY,
    "post_id" INT REFERENCES blog_posts(id) ON DELETE CASCADE NOT NULL,
    "poster_id" INT REFERENCES comment_posters(id) ON DELETE CASCADE NOT NULL,
    "posted_at" TIMESTAMPTZ NOT NULL DEFAULT now(),
    "body" TEXT NOT NULL
);
