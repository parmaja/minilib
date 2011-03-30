--This sample how to create database and use ^ terminator
create table Categories
(
  CatID integer  NOT NULL,
  CatName varchar(200)  NOT NULL,
  constraint pkCategories Primary Key (CatID)
);
^

CREATE UNIQUE INDEX IdxCatName ON Categories (CatName ASC)
^

create table Groups
(
  GrpID integer  NOT NULL,
  GrpName varchar(200)  NOT NULL,
  constraint pkGroups Primary Key (GrpID)
);
^

CREATE UNIQUE INDEX IdxGrpName ON Groups (GrpName ASC)
^

create table Genres
(
  GnrID integer  NOT NULL,
  GnrName varchar(200)  NOT NULL,
  constraint pkGenres Primary Key (GnrID)
);
^

CREATE UNIQUE INDEX IdxGnrName ON Genres (GnrName ASC)
^

create table Plants
(
  PlntID integer  NOT NULL,
  PlntName varchar(200)  NOT NULL,
  PlntLocalName varchar(200) ,
  PlntScienceName integer ,
  PlntNote varchar(200) ,
  PlntGenre integer ,
  PlntCategory integer ,
  PlntGroup integer ,
  constraint pkPlants Primary Key (PlntID),
  constraint fkPlntGenre foreign key (PlntGenre)  REFERENCES Genres (GnrID) ON UPDATE CASCADE ON DELETE SET DEFAULT,
  constraint fkPlntCategory foreign key (PlntCategory)  REFERENCES Categories (CatID) ON UPDATE CASCADE ON DELETE SET DEFAULT,
  constraint fkPlntGroup foreign key (PlntGroup)  REFERENCES Groups (GrpID) ON UPDATE CASCADE ON DELETE SET DEFAULT
);
^

CREATE  INDEX IdxPlntGenre ON Plants (PlntGenre ASC)
^

CREATE  INDEX IdxPlntCategory ON Plants (PlntCategory ASC)
^

CREATE  INDEX IdxPlntGroup ON Plants (PlntGroup ASC)
^
