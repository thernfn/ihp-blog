ALTER TABLE posts ADD COLUMN created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL;
CREATE INDEX posts_created_at_index ON posts (created_at);
