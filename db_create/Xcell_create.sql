-- Created by Vertabelo (http://vertabelo.com)
-- Last modification date: 2018-01-02 09:49:56.574

-- tables
-- Table: cell_type_fk
CREATE TABLE cell_type_fk (
    cell_type varchar(2)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT cell_type_fk_pk PRIMARY KEY (cell_type)
);

-- Table: clima
CREATE TABLE clima (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    param varchar(10)  NOT NULL,
    year decimal(4,0)  NOT NULL,
    month decimal(2,0)  NOT NULL,
    value decimal(6,2)  NOT NULL,
    source varchar(20)  NOT NULL,
    CONSTRAINT clima_ak_1 UNIQUE (site_id, param, year, month, source) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT clima_pk PRIMARY KEY (id)
);

-- Table: clima_data_source_fk
CREATE TABLE clima_data_source_fk (
    source varchar(10)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT clima_data_source_fk_pk PRIMARY KEY (source)
);

-- Table: clima_param_fk
CREATE TABLE clima_param_fk (
    param varchar(10)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT clima_param_fk_pk PRIMARY KEY (param)
);

-- Table: comments
CREATE TABLE comments (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    person_id int  NOT NULL,
    text text  NOT NULL,
    CONSTRAINT comments_ak_1 UNIQUE (site_id, person_id) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT comments_pk PRIMARY KEY (id)
);

-- Table: country_fk
CREATE TABLE country_fk (
    country_code varchar(2)  NOT NULL,
    country varchar(64)  NOT NULL,
    CONSTRAINT country_fk_pk PRIMARY KEY (country_code)
);

-- Table: editing_fk
CREATE TABLE editing_fk (
    editing_level varchar(4)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT editing_fk_pk PRIMARY KEY (editing_level)
);

-- Table: explore_option_summary
CREATE TABLE explore_option_summary (
    id int  NOT NULL,
    site_id int  NOT NULL,
    person_id int  NOT NULL,
    last_name varchar(64)  NOT NULL,
    CONSTRAINT explore_option_summary_pk PRIMARY KEY (id)
);

-- Table: global_table
CREATE TABLE global_table (
    id int  NOT NULL,
    target_proxy varchar(64)  NOT NULL,
    cell_type varchar(64)  NOT NULL,
    organ varchar(64)  NOT NULL,
    site_id int  NOT NULL,
    site_code varchar(5)  NOT NULL,
    country_code varchar(2)  NOT NULL,
    longitude decimal(10,7)  NOT NULL,
    latitude decimal(10,7)  NOT NULL,
    species_code varchar(4)  NOT NULL,
    n_trees int  NOT NULL,
    n_radii_tree int  NOT NULL,
    n_rings int  NOT NULL,
    max_to decimal(4)  NOT NULL,
    max_from decimal(4)  NOT NULL,
    system varchar(10)  NOT NULL,
    software varchar(10)  NOT NULL,
    average_ring_width decimal(8,2)  NOT NULL,
    average_eww decimal(8,2)  NULL,
    average_lww decimal(8,2)  NULL,
    contact_last_name varchar(64)  NOT NULL,
    contact_first_name varchar(64)  NOT NULL,
    contact_email varchar(64)  NOT NULL,
    contact_institution_code varchar(5)  NOT NULL,
    CONSTRAINT global_table_pk PRIMARY KEY (id)
);

-- Table: institution_fk
CREATE TABLE institution_fk (
    institution_code varchar(5)  NOT NULL,
    institution_name varchar(64)  NOT NULL,
    country_code varchar(2)  NOT NULL,
    department varchar(64)  NULL,
    street varchar(64)  NOT NULL,
    postal_code varchar(64)  NULL,
    city varchar(64)  NOT NULL,
    CONSTRAINT institution_fk_pk PRIMARY KEY (institution_code)
);

-- Table: management_fk
CREATE TABLE management_fk (
    management varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT management_fk_pk PRIMARY KEY (management)
);

-- Table: measure_info
CREATE TABLE measure_info (
    id serial  NOT NULL,
    sample_id int  NOT NULL,
    subpiece_label varchar(64)  NOT NULL,
    system varchar(10)  NOT NULL,
    software varchar(10)  NOT NULL,
    software_version varchar(10)  NULL,
    magnification varchar(5)  NULL,
    image_size decimal(6,2)  NULL,
    calibration decimal(12,9)  NULL,
    mes_geometry varchar(2)  NOT NULL,
    only_ew boolean  NOT NULL,
    configuration_filename varchar(128)  NULL,
    data_filename varchar(128)  NULL,
    image_filename varchar(64)  NULL,
    editing_level varchar(4)  NOT NULL,
    "from" decimal(4,0)  NOT NULL,
    "to" decimal(4,0)  NOT NULL,
    CONSTRAINT measure_info_ak_1 UNIQUE (sample_id, subpiece_label, system, software, software_version, magnification, editing_level) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT measure_info_pk PRIMARY KEY (id)
);

CREATE INDEX measure_info_idx_1 on measure_info (id ASC,sample_id ASC,subpiece_label ASC,system ASC,software ASC,software_version ASC,magnification ASC,editing_level ASC);

-- Table: mes_geometry_fk
CREATE TABLE mes_geometry_fk (
    mes_geometry varchar(2)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT mes_geometry_fk_pk PRIMARY KEY (mes_geometry)
);

-- Table: organ_fk
CREATE TABLE organ_fk (
    organ varchar(2)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT organ_fk_pk PRIMARY KEY (organ)
);

-- Table: parenchyma
CREATE TABLE parenchyma (
    id serial  NOT NULL,
    ring_id int  NOT NULL,
    xcal decimal(10,4)  NOT NULL,
    position int  NOT NULL,
    area decimal(10,4)  NOT NULL,
    double_cwtrad decimal(10,4)  NULL,
    CONSTRAINT parenchyma_ak_1 UNIQUE (ring_id, position) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT parenchyma_pk PRIMARY KEY (id)
);

CREATE INDEX parenchyma_idx_1 on parenchyma (id ASC,ring_id ASC);

-- Table: person
CREATE TABLE person (
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

CREATE INDEX person_idx_1 on person (id ASC,last_name ASC,first_name ASC,institution_code ASC);

-- Table: person_role
CREATE TABLE person_role (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    person_id int  NOT NULL,
    role int  NOT NULL,
    CONSTRAINT person_role_ak_1 UNIQUE (site_id, person_id, role) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT person_role_pk PRIMARY KEY (id)
);

-- Table: profile
CREATE TABLE profile (
    id serial  NOT NULL,
    ring_id int  NOT NULL,
    dist int  NOT NULL,
    drad decimal(6,2)  NULL,
    dtan decimal(6,2)  NULL,
    ldrad decimal(6,2)  NULL,
    ldtan decimal(6,2)  NULL,
    cwtrad decimal(6,2)  NULL,
    cwttan decimal(6,2)  NULL,
    lum decimal(10,4)  NULL,
    cwa decimal(10,4)  NULL,
    density decimal(10,4)  NULL,
    CONSTRAINT profile_ak_1 UNIQUE (ring_id, dist) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT profile_pk PRIMARY KEY (id)
);

CREATE INDEX profile_idx_1 on profile (id ASC,ring_id ASC,dist ASC);

-- Table: publication
CREATE TABLE publication (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    reference text  NOT NULL,
    CONSTRAINT publication_ak_1 UNIQUE (site_id, reference) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT publication_pk PRIMARY KEY (id)
);

-- Table: ring
CREATE TABLE ring (
    id serial  NOT NULL,
    subpiece_id int  NOT NULL,
    year decimal(4,0)  NOT NULL,
    ring_width decimal(8,2)  NOT NULL,
    ring_area decimal(10,2)  NULL,
    eww decimal(6,2)  NULL,
    lww decimal(6,2)  NULL,
    ewd decimal(6,2)  NULL,
    lwd decimal(6,2)  NULL,
    mxd decimal(6,2)  NULL,
    mnd decimal(6,2)  NULL,
    rvgi decimal(6,2)  NULL,
    rvsf decimal(6,2)  NULL,
    rgsgv decimal(6,2)  NULL,
    CONSTRAINT ring_ak_1 UNIQUE (subpiece_id, year) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT ring_pk PRIMARY KEY (id)
);

CREATE INDEX ring_idx_1 on ring (id ASC,subpiece_id ASC,year ASC);

-- Table: role_fk
CREATE TABLE role_fk (
    id serial  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT role_fk_pk PRIMARY KEY (id)
);

-- Table: sample_type_fk
CREATE TABLE sample_type_fk (
    sample_type varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT sample_type_fk_pk PRIMARY KEY (sample_type)
);

-- Table: site
CREATE TABLE site (
    id serial  NOT NULL,
    sampling_year int  NOT NULL,
    country_code varchar(2)  NOT NULL,
    site_code varchar(5)  NOT NULL,
    site_label varchar(64)  NOT NULL,
    target_proxy varchar(20)  NOT NULL,
    longitude decimal(10,7)  NOT NULL,
    latitude decimal(10,7)  NOT NULL,
    elevation int  NOT NULL,
    aspect int  NOT NULL,
    slope int  NOT NULL,
    soil_depth varchar(20)  NOT NULL,
    soil_water_capacity varchar(20)  NOT NULL,
    species_composition varchar(20)  NOT NULL,
    management varchar(20)  NOT NULL,
    data_location text  NULL,
    image_location text  NULL,
    sample_location text  NULL,
    CONSTRAINT site_ak_1 UNIQUE (sampling_year, country_code, site_code, target_proxy) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT site_pk PRIMARY KEY (id)
);

CREATE INDEX site_idx_1 on site (id ASC,sampling_year ASC,country_code ASC,site_code ASC,target_proxy ASC);

-- Table: software_fk
CREATE TABLE software_fk (
    software varchar(10)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT software_fk_pk PRIMARY KEY (software)
);

-- Table: soil_depth_fk
CREATE TABLE soil_depth_fk (
    soil_depth varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT soil_depth_fk_pk PRIMARY KEY (soil_depth)
);

-- Table: soil_water_fk
CREATE TABLE soil_water_fk (
    soil_water_capacity varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT soil_water_fk_pk PRIMARY KEY (soil_water_capacity)
);

-- Table: species_composition_fk
CREATE TABLE species_composition_fk (
    species_composition varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT species_composition_fk_pk PRIMARY KEY (species_composition)
);

-- Table: species_fk
CREATE TABLE species_fk (
    species_code varchar(4)  NOT NULL,
    species varchar(64)  NOT NULL,
    CONSTRAINT species_fk_pk PRIMARY KEY (species_code)
);

-- Table: status_fk
CREATE TABLE status_fk (
    status_code varchar(4)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT status_fk_pk PRIMARY KEY (status_code)
);

-- Table: system_fk
CREATE TABLE system_fk (
    system varchar(10)  NOT NULL,
    description text  NOT NULL,
    CONSTRAINT system_fk_pk PRIMARY KEY (system)
);

-- Table: target_proxy_fk
CREATE TABLE target_proxy_fk (
    target_proxy varchar(20)  NOT NULL,
    description varchar(64)  NOT NULL,
    CONSTRAINT target_proxy_fk_pk PRIMARY KEY (target_proxy)
);

-- Table: tracheid_full
CREATE TABLE tracheid_full (
    id serial  NOT NULL,
    ring_id int  NOT NULL,
    x_cal decimal(10,4)  NOT NULL,
    y_cal decimal(10,4)  NOT NULL,
    asp decimal(6,2)  NULL,
    majax decimal(6,2)  NULL,
    dist decimal(8,2)  NULL,
    rel_dist decimal(6,2)  NULL,
    drad decimal(6,2)  NULL,
    dtan decimal(6,2)  NULL,
    ldrad decimal(6,2)  NULL,
    ldtan decimal(6,2)  NULL,
    cwtpi decimal(6,2)  NULL,
    cwtba decimal(6,2)  NULL,
    cwtle decimal(6,2)  NULL,
    cwtri decimal(6,2)  NULL,
    cwttan decimal(6,2)  NULL,
    cwtrad decimal(6,2)  NULL,
    lum decimal(10,4)  NULL,
    cwa decimal(10,4)  NULL,
    bend decimal(10,4)  NULL,
    CONSTRAINT tracheid_full_ak_1 UNIQUE (ring_id, x_cal, y_cal) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT tracheid_full_pk PRIMARY KEY (id)
);

CREATE INDEX tracheid_full_idx_1 on tracheid_full (id ASC,ring_id ASC);

-- Table: tracheid_row
CREATE TABLE tracheid_row (
    id serial  NOT NULL,
    ring_id int  NOT NULL,
    row int  NOT NULL,
    position int  NOT NULL,
    ldrad decimal(6,2)  NOT NULL,
    double_cwtrad decimal(6,2)  NULL,
    lum decimal(10,4)  NULL,
    CONSTRAINT tracheid_row_ak_1 UNIQUE (ring_id, row, position) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT tracheid_row_pk PRIMARY KEY (id)
);

CREATE INDEX tracheid_row_idx_1 on tracheid_row (id ASC,ring_id ASC,row ASC,position ASC);

-- Table: tree
CREATE TABLE tree (
    id serial  NOT NULL,
    site_id int  NOT NULL,
    tree_label varchar(64)  NOT NULL,
    species_code varchar(4)  NOT NULL,
    status_code varchar(4)  NOT NULL,
    dbh decimal(6,2)  NULL,
    height decimal(4,2)  NULL,
    age int  NULL,
    CONSTRAINT tree_ak_1 UNIQUE (site_id, tree_label, species_code) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT tree_pk PRIMARY KEY (id)
);

CREATE INDEX tree_idx_1 on tree (id ASC,site_id ASC,tree_label ASC,species_code ASC);

-- Table: vessel
CREATE TABLE vessel (
    id serial  NOT NULL,
    ring_id int  NOT NULL,
    x_cal decimal(10,4)  NOT NULL,
    y_cal decimal(10,4)  NOT NULL,
    asp decimal(6,2)  NULL,
    majax decimal(6,2)  NULL,
    nbrno decimal(6,2)  NULL,
    nbrdst decimal(6,2)  NULL,
    dist decimal(8,2)  NULL,
    rel_dist decimal(6,2)  NULL,
    lum decimal(10,4)  NULL,
    CONSTRAINT vessel_ak_1 UNIQUE (ring_id, x_cal, y_cal) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT vessel_pk PRIMARY KEY (id)
);

CREATE INDEX vessel_idx_1 on vessel (id ASC,ring_id ASC,x_cal ASC,y_cal ASC);

-- Table: wood_sample
CREATE TABLE wood_sample (
    id serial  NOT NULL,
    tree_id int  NOT NULL,
    sample_label varchar(64)  NOT NULL,
    sample_type varchar(20)  NOT NULL,
    radius varchar(2)  NOT NULL,
    organ varchar(2)  NOT NULL,
    cell_type varchar(2)  NOT NULL,
    stem_height decimal(6,2)  NULL,
    apex_dist decimal(6,2)  NULL,
    CONSTRAINT wood_sample_ak_1 UNIQUE (tree_id, sample_label, radius, organ, cell_type) NOT DEFERRABLE  INITIALLY IMMEDIATE,
    CONSTRAINT wood_sample_pk PRIMARY KEY (id)
);

CREATE INDEX wood_sample_idx_1 on wood_sample (id ASC,tree_id ASC,sample_label ASC,radius ASC,organ ASC,sample_type ASC);

-- foreign keys
-- Reference: Archiving_wood_sample (table: measure_info)
ALTER TABLE measure_info ADD CONSTRAINT Archiving_wood_sample
    FOREIGN KEY (sample_id)
    REFERENCES wood_sample (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Cell_ring (table: profile)
ALTER TABLE profile ADD CONSTRAINT Cell_ring
    FOREIGN KEY (ring_id)
    REFERENCES ring (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Core_Tree (table: wood_sample)
ALTER TABLE wood_sample ADD CONSTRAINT Core_Tree
    FOREIGN KEY (tree_id)
    REFERENCES tree (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Core_cell_type_fk (table: wood_sample)
ALTER TABLE wood_sample ADD CONSTRAINT Core_cell_type_fk
    FOREIGN KEY (cell_type)
    REFERENCES cell_type_fk (cell_type)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Core_type_fk (table: wood_sample)
ALTER TABLE wood_sample ADD CONSTRAINT Core_type_fk
    FOREIGN KEY (organ)
    REFERENCES organ_fk (organ)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Tree_site (table: tree)
ALTER TABLE tree ADD CONSTRAINT Tree_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Tree_species_fk (table: tree)
ALTER TABLE tree ADD CONSTRAINT Tree_species_fk
    FOREIGN KEY (species_code)
    REFERENCES species_fk (species_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: Tree_status_fk (table: tree)
ALTER TABLE tree ADD CONSTRAINT Tree_status_fk
    FOREIGN KEY (status_code)
    REFERENCES status_fk (status_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: clima_clima_data_source_fk (table: clima)
ALTER TABLE clima ADD CONSTRAINT clima_clima_data_source_fk
    FOREIGN KEY (source)
    REFERENCES clima_data_source_fk (source)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: clima_clima_factor_fk (table: clima)
ALTER TABLE clima ADD CONSTRAINT clima_clima_factor_fk
    FOREIGN KEY (param)
    REFERENCES clima_param_fk (param)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: clima_site (table: clima)
ALTER TABLE clima ADD CONSTRAINT clima_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: comments_person (table: comments)
ALTER TABLE comments ADD CONSTRAINT comments_person
    FOREIGN KEY (person_id)
    REFERENCES person (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: comments_site (table: comments)
ALTER TABLE comments ADD CONSTRAINT comments_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: contributor_institution_fk (table: person)
ALTER TABLE person ADD CONSTRAINT contributor_institution_fk
    FOREIGN KEY (institution_code)
    REFERENCES institution_fk (institution_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_contribution_person_role (table: person_role)
ALTER TABLE person_role ADD CONSTRAINT data_contribution_person_role
    FOREIGN KEY (role)
    REFERENCES role_fk (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_owners_person (table: person_role)
ALTER TABLE person_role ADD CONSTRAINT data_owners_person
    FOREIGN KEY (person_id)
    REFERENCES person (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: data_owners_site (table: person_role)
ALTER TABLE person_role ADD CONSTRAINT data_owners_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: explore_option_summary_person (table: explore_option_summary)
ALTER TABLE explore_option_summary ADD CONSTRAINT explore_option_summary_person
    FOREIGN KEY (person_id)
    REFERENCES person (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: explore_option_summary_site (table: explore_option_summary)
ALTER TABLE explore_option_summary ADD CONSTRAINT explore_option_summary_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: institution_fk_country_fk (table: institution_fk)
ALTER TABLE institution_fk ADD CONSTRAINT institution_fk_country_fk
    FOREIGN KEY (country_code)
    REFERENCES country_fk (country_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: measure_info_editing_fk (table: measure_info)
ALTER TABLE measure_info ADD CONSTRAINT measure_info_editing_fk
    FOREIGN KEY (editing_level)
    REFERENCES editing_fk (editing_level)  
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: measure_info_mes_geometry_fk (table: measure_info)
ALTER TABLE measure_info ADD CONSTRAINT measure_info_mes_geometry_fk
    FOREIGN KEY (mes_geometry)
    REFERENCES mes_geometry_fk (mes_geometry)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: measure_info_software_fk (table: measure_info)
ALTER TABLE measure_info ADD CONSTRAINT measure_info_software_fk
    FOREIGN KEY (software)
    REFERENCES software_fk (software)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: measure_info_system_fk (table: measure_info)
ALTER TABLE measure_info ADD CONSTRAINT measure_info_system_fk
    FOREIGN KEY (system)
    REFERENCES system_fk (system)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: parenchyma_ring (table: parenchyma)
ALTER TABLE parenchyma ADD CONSTRAINT parenchyma_ring
    FOREIGN KEY (ring_id)
    REFERENCES ring (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: publication_site (table: publication)
ALTER TABLE publication ADD CONSTRAINT publication_site
    FOREIGN KEY (site_id)
    REFERENCES site (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: ring_measure_info (table: ring)
ALTER TABLE ring ADD CONSTRAINT ring_measure_info
    FOREIGN KEY (subpiece_id)
    REFERENCES measure_info (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_Management_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_Management_fk
    FOREIGN KEY (management)
    REFERENCES management_fk (management)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_country_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_country_fk
    FOREIGN KEY (country_code)
    REFERENCES country_fk (country_code)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_soil_depth_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_soil_depth_fk
    FOREIGN KEY (soil_depth)
    REFERENCES soil_depth_fk (soil_depth)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_soil_water_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_soil_water_fk
    FOREIGN KEY (soil_water_capacity)
    REFERENCES soil_water_fk (soil_water_capacity)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_species_composition_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_species_composition_fk
    FOREIGN KEY (species_composition)
    REFERENCES species_composition_fk (species_composition)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: site_target_proxy_fk (table: site)
ALTER TABLE site ADD CONSTRAINT site_target_proxy_fk
    FOREIGN KEY (target_proxy)
    REFERENCES target_proxy_fk (target_proxy)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: tracheid_full_ring (table: tracheid_full)
ALTER TABLE tracheid_full ADD CONSTRAINT tracheid_full_ring
    FOREIGN KEY (ring_id)
    REFERENCES ring (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: tracheid_row_ring (table: tracheid_row)
ALTER TABLE tracheid_row ADD CONSTRAINT tracheid_row_ring
    FOREIGN KEY (ring_id)
    REFERENCES ring (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: vessel_ring (table: vessel)
ALTER TABLE vessel ADD CONSTRAINT vessel_ring
    FOREIGN KEY (ring_id)
    REFERENCES ring (id)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- Reference: wood_sample_Copy_of_cell_type_fk (table: wood_sample)
ALTER TABLE wood_sample ADD CONSTRAINT wood_sample_Copy_of_cell_type_fk
    FOREIGN KEY (sample_type)
    REFERENCES sample_type_fk (sample_type)
    ON DELETE  RESTRICT 
    ON UPDATE  CASCADE 
    NOT DEFERRABLE 
    INITIALLY IMMEDIATE
;

-- End of file.

