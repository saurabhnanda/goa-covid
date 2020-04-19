create table users
( id serial primary key
, username text not null
, password text not null
, invitation_link text
, invitation_response jsonb
, account_created bool not null default false
, created_at timestamp with time zone not null default now()
, admin1 text
, admin2 text
, batch_key text
);

create unique index on users(lower(username));


create table forms
( id serial primary key
, zoho_form_id text not null
, link_name text not null
);

create unique index on forms(zoho_form_id);
create unique index on forms(link_name);


create table users_forms
( username text not null
, link_name text not null
);

create unique index on users_forms(username, link_name);
-- create index on users_forms(is_shared);
       

alter table users add column is_respondent boolean not null default false;
alter table users add column zoho_user_id text ;
