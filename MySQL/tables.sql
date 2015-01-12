CREATE TABLE `gsod_stations` (
  `station_id` int(11) NOT NULL,
  `pixel_1d` mediumint(9) NOT NULL,
  `station_code` char(12) NOT NULL,
  `stationname` varchar(55) DEFAULT NULL,
  `ctry` char(2) DEFAULT NULL,
  `fips` char(2) DEFAULT NULL,
  `state` char(2) DEFAULT NULL,
  `call` varchar(25) DEFAULT NULL,
  `lat` decimal(6,3) NOT NULL,
  `lon` decimal(6,3) NOT NULL,
  `elev1m` decimal(10,3) DEFAULT NULL,
  `begin` date DEFAULT NULL,
  `end` date DEFAULT NULL,
  PRIMARY KEY (`station_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `gsod_xd` (
  `station_id` int(11) NOT NULL,
  `wdate` date NOT NULL,
  `tavg` int(11) DEFAULT NULL,
  `slpressure` int(11) DEFAULT NULL,
  `stpressure` int(11) DEFAULT NULL,
  `tdew` int(11) DEFAULT NULL,
  `visibility` int(11) DEFAULT NULL,
  `wind` int(11) DEFAULT NULL,
  `maxwind` int(11) DEFAULT NULL,
  `gust` int(11) DEFAULT NULL,
  `tmax` int(11) DEFAULT NULL,
  `tmin` int(11) DEFAULT NULL,
  `prec` int(11) DEFAULT NULL,
  `snowdepth` int(11) DEFAULT NULL,
  `ifog` tinyint(1) DEFAULT NULL,
  `irain` tinyint(1) DEFAULT NULL,
  `isnow` tinyint(1) DEFAULT NULL,
  `ihail` tinyint(1) DEFAULT NULL,
  `ithunder` tinyint(1) DEFAULT NULL,
  `itornado` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`wdate`,`station_id`),
  KEY `sd_idx` (`station_id`,`wdate`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `nasa_1d` (
  `cell` mediumint(9) NOT NULL,
  `wdate` date NOT NULL,
  `toa_dwn` mediumint(9) DEFAULT NULL,
  `srad` mediumint(9) DEFAULT NULL,
  `lwv_dwn` mediumint(9) DEFAULT NULL,
  `tavg` mediumint(9) DEFAULT NULL,
  `tmin` mediumint(9) DEFAULT NULL,
  `tmax` mediumint(9) DEFAULT NULL,
  `rh2m` mediumint(9) DEFAULT NULL,
  `tdew` mediumint(9) DEFAULT NULL,
  `prec` mediumint(9) DEFAULT NULL,
  `wind` mediumint(9) DEFAULT NULL,
  PRIMARY KEY (`wdate`,`cell`),
  KEY `cdt_idx` (`cell`,`wdate`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `nasacorr_1d` (
  `cell` mediumint(6) NOT NULL,
  `wdate` date NOT NULL,
  `tmin` mediumint(9) DEFAULT NULL,
  `tmax` mediumint(9) DEFAULT NULL,
  `tdew` mediumint(9) DEFAULT NULL,
  PRIMARY KEY (`wdate`,`cell`),
  KEY `cw_idx` (`cell`,`wdate`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
