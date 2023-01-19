ALTER TABLE comments ADD COLUMN created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL;
CREATE INDEX comments_created_at_index ON comments (created_at);
