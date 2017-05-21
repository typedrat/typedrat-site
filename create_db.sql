CREATE TABLE blog_posts (
    "id" SERIAL PRIMARY KEY,
    "title" TEXT NOT NULL,
    "posted_at" TIMESTAMPTZ NOT NULL DEFAULT now(),
    "body" TEXT NOT NULL,
    "slug" TEXT NOT NULL
);

CREATE TABLE users (
    "id" SERIAL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "auth_token" TEXT,
    "profile_url" TEXT NOT NULL,
    "avatar_url" TEXT NOT NULL
);

CREATE TABLE comments (
    "id" SERIAL PRIMARY KEY,
    "post_id" INT REFERENCES blog_posts(id) ON DELETE CASCADE NOT NULL,
    "author_id" INT REFERENCES users(id) ON DELETE CASCADE NOT NULL,
    "posted_at" TIMESTAMPTZ NOT NULL DEFAULT now(),
    "body" TEXT NOT NULL
);
