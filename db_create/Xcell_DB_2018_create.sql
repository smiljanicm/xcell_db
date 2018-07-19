-- Created by Vertabelo (http://vertabelo.com)
-- Last modification date: 2018-07-19 13:49:31.516

-- tables
-- Table: cell
CREATE TABLE v1.cell (
    ring_id int  NOT NULL,
    x_cal decimal(10,4)  NOT NULL,
    y_cal decimal(12,4)  NOT NULL,
    param_id int  NOT NULL,
    value decimal(10,4)  NOT NULL,
    CONSTRAINT cell_pk PRIMARY KEY (ring_id,x_cal,y_cal,param_id)
);

CREATE INDEX cell_idx_1 on v1.cell (param_id ASC,ring_id ASC);

-- Table: comments
CREATE TABLE v1.comments (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    person_id int  NOT NULL,
    text text  NOT NULL,
    CONSTRAINT comments_ak_1 UNIQUE (site_id, person_id) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT comments_pk PRIMARY KEY (id)
);

-- Table: country_fk
CREATE TABLE v1.country_fk (
    country_code varchar(2)  NOT NULL,
    country varchar(64)  NOT NULL,
    CONSTRAINT country_fk_pk PRIMARY KEY (country_code)
);

-- Table: global_table
CREATE TABLE v1.global_table (
    site_code varchar(5)  NOT NULL,
    year int  NOT NULL,
    site_label varchar(64)  NOT NULL,
    manip varchar(5)  NOT NULL,
    country_code varchar(2)  NOT NULL,
    longitude decimal(10,7)  NOT NULL,
    latitude decimal(10,7)  NOT NULL,
    elevation int  NOT NULL,
    temp decimal(6,2)  NOT NULL,
    prec decimal(6,2)  NOT NULL,
    species_code varchar(4)  NOT NULL,
    wood_type varchar(124)  NOT NULL,
    leaf_habit varchar(124)  NOT NULL,
    wood_plane varchar(124)  NOT NULL,
    organ varchar(2)  NOT NULL,
    output varchar(64)  NOT NULL,
    hardware varchar(64)  NOT NULL,
    software varchar(64)  NOT NULL,
    last_name varchar(64)  NOT NULL,
    first_name varchar(64)  NOT NULL,
    email varchar(64)  NOT NULL,
    institution_code varchar(5)  NOT NULL,
    n_trees int  NOT NULL,
    n_radii int  NOT NULL,
    n_rings int  NOT NULL,
    "from" int  NOT NULL,
    "to" int  NOT NULL,
    ring_width decimal(10,4)  NULL,
    la decimal(10,4)  NULL,
    ldrad decimal(10,4)  NULL,
    ldtan decimal(10,4)  NULL,
    cwtrad decimal(10,4)  NULL,
    cwttan decimal(10,4)  NULL,
    cwa decimal(10,4)  NULL,
    CONSTRAINT global_table_pk PRIMARY KEY (site_code,year,manip,species_code,organ,output,hardware,software)
);

-- Table: institution_fk
CREATE TABLE v1.institution_fk (
    institution_code varchar(5)  NOT NULL,
    institution_name varchar(64)  NOT NULL,
    country_code varchar(2)  NOT NULL,
    department varchar(128)  NULL,
    street varchar(64)  NULL,
    postal_code varchar(64)  NULL,
    city varchar(64)  NOT NULL,
    CONSTRAINT institution_fk_ak_1 UNIQUE (institution_name) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT institution_fk_pk PRIMARY KEY (institution_code)
);

-- Table: meas_met_fk
CREATE TABLE v1.meas_met_fk (
    id serial  NOT NULL,
    output varchar(64)  NOT NULL,
    hardware varchar(64)  NOT NULL,
    software varchar(64)  NOT NULL,
    CONSTRAINT meas_met_fk_ak_1 UNIQUE (output, hardware, software) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT meas_met_fk_pk PRIMARY KEY (id)
);

-- Table: meas_met_param_fk
CREATE TABLE v1.meas_met_param_fk (
    id serial  NOT NULL,
    parameter varchar(64)  NOT NULL,
    unit varchar(20)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT meas_met_param_fk_ak_1 UNIQUE (parameter) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT meas_met_param_fk_pk PRIMARY KEY (id)
);

CREATE INDEX meas_met_param_fk_idx_1 on v1.meas_met_param_fk (parameter ASC,description ASC);

-- Table: meas_met_set
CREATE TABLE v1.meas_met_set (
    subsample_id int  NOT NULL,
    meas_met_param_id int  NOT NULL,
    value varchar(124)  NOT NULL,
    CONSTRAINT meas_met_set_pk PRIMARY KEY (subsample_id,meas_met_param_id,value)
);

CREATE INDEX meas_met_set_idx_1 on v1.meas_met_set (meas_met_param_id ASC);

-- Table: meas_param_fk
CREATE TABLE v1.meas_param_fk (
    id serial  NOT NULL,
    parameter varchar(32)  NOT NULL,
    unit varchar(20)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT meas_param_fk_pk PRIMARY KEY (id)
);

CREATE INDEX tracheid_full_idx_1 on v1.meas_param_fk (parameter ASC);

-- Table: organ_fk
CREATE TABLE v1.organ_fk (
    organ varchar(2)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT organ_fk_pk PRIMARY KEY (organ)
);

-- Table: person
CREATE TABLE v1.person (
    id serial  NOT NULL,
    last_name varchar(64)  NOT NULL,
    first_name varchar(64)  NOT NULL,
    email varchar(64)  NOT NULL,
    institution_code varchar(5)  NOT NULL,
    webpage varchar(128)  NULL,
    phone_number varchar(64)  NULL,
    CONSTRAINT person_ak_1 UNIQUE (last_name, first_name, institution_code) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT person_pk PRIMARY KEY (id)
);

CREATE INDEX person_idx_1 on v1.person (id ASC,last_name ASC,first_name ASC,institution_code ASC);

-- Table: person_role
CREATE TABLE v1.person_role (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    person_id int  NOT NULL,
    role int  NOT NULL,
    CONSTRAINT person_role_ak_1 UNIQUE (site_id, person_id, role) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT person_role_pk PRIMARY KEY (id)
);

-- Table: publication
CREATE TABLE v1.publication (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    first_author_last_name varchar(25)  NOT NULL,
    title text  NOT NULL,
    year decimal(4,0)  NOT NULL,
    journal varchar(64)  NOT NULL,
    doi varchar(64)  NOT NULL,
    CONSTRAINT publication_ak_1 UNIQUE (site_id, doi) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT publication_pk PRIMARY KEY (id)
);

CREATE INDEX publication_idx_1 on v1.publication (id ASC);

-- Table: ring
CREATE TABLE v1.ring (
    id serial  NOT NULL,
    subsample_id int  NOT NULL,
    year decimal(4,0)  NOT NULL,
    CONSTRAINT ring_ak_1 UNIQUE (subsample_id, year) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT ring_pk PRIMARY KEY (id)
);

CREATE INDEX ring_idx_1 on v1.ring (subsample_id ASC,year ASC,id ASC);

-- Table: role_fk
CREATE TABLE v1.role_fk (
    id serial  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT role_fk_pk PRIMARY KEY (id)
);

-- Table: sample
CREATE TABLE v1.sample (
    id serial  NOT NULL,
    tree_id int  NOT NULL,
    sample_label varchar(64)  NOT NULL,
    organ varchar(6)  NOT NULL,
    CONSTRAINT wood_sample_ak_1 UNIQUE (tree_id, sample_label, organ) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT sample_pk PRIMARY KEY (id)
);

CREATE INDEX wood_sample_idx_1 on v1.sample (id ASC,tree_id ASC,sample_label ASC,organ ASC);

-- Table: sample_param
CREATE TABLE v1.sample_param (
    sample_id int  NOT NULL,
    sample_param_id int  NOT NULL,
    value varchar(124)  NOT NULL,
    CONSTRAINT sample_param_pk PRIMARY KEY (sample_id,sample_param_id)
);

CREATE INDEX sample_param_idx_1 on v1.sample_param (sample_param_id ASC);

-- Table: sample_param_fk
CREATE TABLE v1.sample_param_fk (
    id serial  NOT NULL,
    parameter varchar(32)  NOT NULL,
    unit varchar(20)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT sample_param_fk_ak_1 UNIQUE (parameter) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT sample_param_fk_pk PRIMARY KEY (id)
);

-- Table: site
CREATE TABLE v1.site (
    id serial  NOT NULL,
    sampling_year int  NOT NULL,
    country_code varchar(2)  NOT NULL,
    site_code varchar(5)  NOT NULL,
    site_label varchar(64)  NOT NULL,
    longitude decimal(10,7)  NOT NULL,
    latitude decimal(10,7)  NOT NULL,
    elevation int  NOT NULL,
    temp decimal(6,2)  NOT NULL,
    prec decimal(6,2)  NOT NULL,
    CONSTRAINT site_ak_1 UNIQUE (sampling_year, country_code, site_code) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT site_pk PRIMARY KEY (id)
);

CREATE INDEX site_idx_1 on v1.site (id ASC,sampling_year ASC,country_code ASC,site_code ASC);

-- Table: site_param
CREATE TABLE v1.site_param (
    site_id int  NOT NULL,
    site_param_id int  NOT NULL,
    value varchar(124)  NOT NULL,
    CONSTRAINT site_param_pk PRIMARY KEY (site_id,site_param_id)
);

CREATE INDEX site_param_idx_1 on v1.site_param (site_param_id ASC);

-- Table: site_param_fk
CREATE TABLE v1.site_param_fk (
    id serial  NOT NULL,
    parameter varchar(32)  NOT NULL,
    unit varchar(10)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT site_param_fk_ak_1 UNIQUE (parameter) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT site_param_fk_pk PRIMARY KEY (id)
);

-- Table: species_fk
CREATE TABLE v1.species_fk (
    species_code varchar(4)  NOT NULL,
    species varchar(128)  NOT NULL,
    common_name varchar(128)  NULL,
    wood_type varchar(10)  NULL,
    leaf_habit varchar(9)  NULL,
    wood_plane varchar(14)  NULL,
    CONSTRAINT species_fk_pk PRIMARY KEY (species_code)
);

-- Table: subsample
CREATE TABLE v1.subsample (
    id serial  NOT NULL,
    sample_id int  NOT NULL,
    subpiece_label varchar(64)  NOT NULL,
    meas_met_id int  NOT NULL,
    meas_date date  NOT NULL,
    CONSTRAINT measure_info_ak_1 UNIQUE (sample_id, subpiece_label, meas_met_id, meas_date) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT subsample_pk PRIMARY KEY (id)
);

CREATE INDEX measure_info_idx_1 on v1.subsample (sample_id ASC,meas_met_id ASC,id ASC);

-- Table: tree
CREATE TABLE v1.tree (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    tree_label varchar(64)  NOT NULL,
    species_code varchar(4)  NOT NULL,
    CONSTRAINT tree_ak_1 UNIQUE (site_id, tree_label, species_code) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT tree_pk PRIMARY KEY (id)
);

CREATE INDEX tree_idx_1 on v1.tree (id ASC,site_id ASC,tree_label ASC,species_code ASC);

-- Table: tree_param
CREATE TABLE v1.tree_param (
    tree_id int  NOT NULL,
    tree_param_id int  NOT NULL,
    value varchar(124)  NOT NULL,
    CONSTRAINT tree_param_pk PRIMARY KEY (tree_id,tree_param_id)
);

CREATE INDEX tree_param_idx_1 on v1.tree_param (tree_param_id ASC);

-- Table: tree_param_fk
CREATE TABLE v1.tree_param_fk (
    id serial  NOT NULL,
    parameter varchar(32)  NOT NULL,
    unit varchar(20)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT tree_param_fk_ak_1 UNIQUE (parameter) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT tree_param_fk_pk PRIMARY KEY (id)
);

-- Table: year
CREATE TABLE v1.year (
    ring_id int  NOT NULL,
    param_id int  NOT NULL,
    value decimal(10,4)  NOT NULL,
    CONSTRAINT year_pk PRIMARY KEY (ring_id,param_id)
);

CREATE INDEX year_idx_1 on v1.year (param_id ASC,ring_id ASC);

-- foreign keys
-- Reference: Archiving_wood_sample (table: subsample)
ALTER TABLE v1.subsample ADD CONSTRAINT Archiving_wood_sample
    FOREIGN KEY (sample_id)
    REFERENCES v1.sample (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Copy_of_site_param_tree (table: tree_param)
ALTER TABLE v1.tree_param ADD CONSTRAINT Copy_of_site_param_tree
    FOREIGN KEY (tree_id)
    REFERENCES v1.tree (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Copy_of_site_param_tree_param_fk (table: tree_param)
ALTER TABLE v1.tree_param ADD CONSTRAINT Copy_of_site_param_tree_param_fk
    FOREIGN KEY (tree_param_id)
    REFERENCES v1.tree_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Core_Tree (table: sample)
ALTER TABLE v1.sample ADD CONSTRAINT Core_Tree
    FOREIGN KEY (tree_id)
    REFERENCES v1.tree (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Core_type_fk (table: sample)
ALTER TABLE v1.sample ADD CONSTRAINT Core_type_fk
    FOREIGN KEY (organ)
    REFERENCES v1.organ_fk (organ)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Tree_site (table: tree)
ALTER TABLE v1.tree ADD CONSTRAINT Tree_site
    FOREIGN KEY (site_id)
    REFERENCES v1.site (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Tree_species_fk (table: tree)
ALTER TABLE v1.tree ADD CONSTRAINT Tree_species_fk
    FOREIGN KEY (species_code)
    REFERENCES v1.species_fk (species_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: cell_parameters_fk (table: cell)
ALTER TABLE v1.cell ADD CONSTRAINT cell_parameters_fk
    FOREIGN KEY (param_id)
    REFERENCES v1.meas_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: comments_person (table: comments)
ALTER TABLE v1.comments ADD CONSTRAINT comments_person
    FOREIGN KEY (person_id)
    REFERENCES v1.person (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: comments_site (table: comments)
ALTER TABLE v1.comments ADD CONSTRAINT comments_site
    FOREIGN KEY (site_id)
    REFERENCES v1.site (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: contributor_institution_fk (table: person)
ALTER TABLE v1.person ADD CONSTRAINT contributor_institution_fk
    FOREIGN KEY (institution_code)
    REFERENCES v1.institution_fk (institution_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_contribution_person_role (table: person_role)
ALTER TABLE v1.person_role ADD CONSTRAINT data_contribution_person_role
    FOREIGN KEY (role)
    REFERENCES v1.role_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_owners_person (table: person_role)
ALTER TABLE v1.person_role ADD CONSTRAINT data_owners_person
    FOREIGN KEY (person_id)
    REFERENCES v1.person (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_owners_site (table: person_role)
ALTER TABLE v1.person_role ADD CONSTRAINT data_owners_site
    FOREIGN KEY (site_id)
    REFERENCES v1.site (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: institution_fk_country_fk (table: institution_fk)
ALTER TABLE v1.institution_fk ADD CONSTRAINT institution_fk_country_fk
    FOREIGN KEY (country_code)
    REFERENCES v1.country_fk (country_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: meas_met_set_meas_met_param_fk (table: meas_met_set)
ALTER TABLE v1.meas_met_set ADD CONSTRAINT meas_met_set_meas_met_param_fk
    FOREIGN KEY (meas_met_param_id)
    REFERENCES v1.meas_met_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: meas_met_set_subsample (table: meas_met_set)
ALTER TABLE v1.meas_met_set ADD CONSTRAINT meas_met_set_subsample
    FOREIGN KEY (subsample_id)
    REFERENCES v1.subsample (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: publication_site (table: publication)
ALTER TABLE v1.publication ADD CONSTRAINT publication_site
    FOREIGN KEY (site_id)
    REFERENCES v1.site (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: ring_measure_info (table: ring)
ALTER TABLE v1.ring ADD CONSTRAINT ring_measure_info
    FOREIGN KEY (subsample_id)
    REFERENCES v1.subsample (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: ring_year (table: year)
ALTER TABLE v1.year ADD CONSTRAINT ring_year
    FOREIGN KEY (ring_id)
    REFERENCES v1.ring (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: sample_param_sample (table: sample_param)
ALTER TABLE v1.sample_param ADD CONSTRAINT sample_param_sample
    FOREIGN KEY (sample_id)
    REFERENCES v1.sample (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: sample_param_sample_param_fk (table: sample_param)
ALTER TABLE v1.sample_param ADD CONSTRAINT sample_param_sample_param_fk
    FOREIGN KEY (sample_param_id)
    REFERENCES v1.sample_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_country_fk (table: site)
ALTER TABLE v1.site ADD CONSTRAINT site_country_fk
    FOREIGN KEY (country_code)
    REFERENCES v1.country_fk (country_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_param_site (table: site_param)
ALTER TABLE v1.site_param ADD CONSTRAINT site_param_site
    FOREIGN KEY (site_id)
    REFERENCES v1.site (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_param_site_param_fk (table: site_param)
ALTER TABLE v1.site_param ADD CONSTRAINT site_param_site_param_fk
    FOREIGN KEY (site_param_id)
    REFERENCES v1.site_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: subpiece_meas_met_fk (table: subsample)
ALTER TABLE v1.subsample ADD CONSTRAINT subpiece_meas_met_fk
    FOREIGN KEY (meas_met_id)
    REFERENCES v1.meas_met_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: tracheid_full_ring (table: cell)
ALTER TABLE v1.cell ADD CONSTRAINT tracheid_full_ring
    FOREIGN KEY (ring_id)
    REFERENCES v1.ring (id)
    ON DELETE  CASCADE 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: year_meas_param_fk (table: year)
ALTER TABLE v1.year ADD CONSTRAINT year_meas_param_fk
    FOREIGN KEY (param_id)
    REFERENCES v1.meas_param_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- End of file.

