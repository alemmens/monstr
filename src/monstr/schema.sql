-- Careful with edits here. Our poor-man's parser expects each ddl statement
-- to be separated by one or more full comment lines.
--
pragma encoding = 'UTF-8';
--
pragma journal_mode = WAL;
--
pragma main.synchronous = NORMAL;
--
pragma foreign_keys = OFF;
--
create table if not exists n_events
(
    id              varchar(64) not null unique,
    pubkey          varchar(64) not null,
    created_at      integer     not null,
    kind            integer     not null,
    content_        text        not null,
    raw_event_tuple text        not null,
    deleted_        integer     not null default 0,
    sys_ts          timestamp            default current_timestamp
);
--
create index if not exists idx_event_id on n_events (id);
--
create index if not exists idx_pubkey on n_events (pubkey);
--
create index if not exists idx_created_at on n_events (created_at);
--
create index if not exists idx_kind on n_events (kind);
--
create index if not exists idx_kind_pubkey on n_events (kind, pubkey);
--
create table if not exists e_tags
(
    source_event_id varchar(64) not null,
    tagged_event_id varchar(64) not null,
    unique (source_event_id, tagged_event_id)
);
--
create index if not exists idx_tagged_event_id on e_tags (tagged_event_id);
--
create table if not exists p_tags
(
    source_event_id varchar(64) not null,
    tagged_pubkey   varchar(64) not null,
    unique (source_event_id, tagged_pubkey)
);
--
create index if not exists idx_tagged_pubkey on p_tags (tagged_pubkey);
--
create table if not exists identities_
(
    public_key varchar(64) not null,
    secret_key varchar(64) null,
    unique (public_key)
);
--
create table if not exists relays_
(
    url    varchar(64) not null,
    read_  integer     not null default 0,
    write_ integer     not null default 0,
    unique (url)
);
--
create table if not exists relay_state
(
    url    varchar(64) not null,
    unique (url)
);
--
create table if not exists relay_event_id
(
    event_id   varchar(64) not null,
    relay_url  varchar(64) not null,
    unique (event_id, relay_url)
);
--
create table if not exists signature_event_id
(
    event_id   varchar(64) not null unique,
    signature_ varchar(64) not null,
    unique (event_id)
);
--