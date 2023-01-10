CREATE TABLE optim.jurisdiction_abbrev_ref (
 abbrevref_id int PRIMARY KEY,
 name text NOT NULL,
 info jsonb NOT NULL
);

COMMENT ON COLUMN optim.jurisdiction_abbrev_ref.abbrevref_id IS 'Source identifier.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_ref.name         IS 'Source name.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_ref.info         IS 'Others information.';

COMMENT ON TABLE optim.jurisdiction_abbrev_ref IS 'Source of abbreviations.';

CREATE TABLE optim.jurisdiction_abbrev_option (
 selected boolean NOT NULL DEFAULT false,
 abbrevref_id int NOT NULL REFERENCES optim.jurisdiction_abbrev_ref,
 isolabel_ext text NOT NULL,
 abbrev text NOT NULL,
 insert_date date NOT NULL default now(),
 PRIMARY KEY (abbrevref_id,isolabel_ext,insert_date)
);

COMMENT ON COLUMN optim.jurisdiction_abbrev_option.selected     IS 'Standard jurisdiction abbreviation.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_option.abbrevref_id IS 'optim.jurisdiction_abbrev_ref primary key referencek.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_option.isolabel_ext IS 'ISO and name (camel case), e.g. BR-SP-SaoPaulo.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_option.abbrev       IS 'Abbreviation.';
COMMENT ON COLUMN optim.jurisdiction_abbrev_option.insert_date  IS 'Date the abbreviation was added..';

COMMENT ON TABLE optim.jurisdiction_abbrev_option IS 'Stores abbreviations for a jurisdiction.';
