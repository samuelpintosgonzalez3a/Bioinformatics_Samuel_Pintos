-- UNIR | Máster en Bioinformática | Bases de Datos y Recursos Bioinformáticos 
-- Taller grupal. Creación de una base de datos genómica

-- Grupo 27: Rita Pellissa Valera, Samuel Pintos González, Tamara Noya Mosquera, 
-- Vanesa de las Heras Hermosilla, Virginia García-Loygorri Arias y Yannis Avlonitis Egea.



-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `grupo27` DEFAULT CHARACTER SET UTF8MB4 ;
USE `grupo27`;



-- -----------------------------------------------------
-- Table `grupo27`.`gen`
-- -----------------------------------------------------

CREATE TABLE IF NOT EXISTS `grupo27`.`gen` (
    `G_id_gen` VARCHAR(45) NOT NULL,
    `G_cromosoma` INT(2) NOT NULL,
    `G_nombre_gen` VARCHAR(45) NOT NULL,
    `G_posicion_gen` VARCHAR(45) NOT NULL,
    `G_descripcion_gen`VARCHAR(100) NOT NULL,
    PRIMARY KEY (G_id_gen)
)  ENGINE=INNODB;

INSERT INTO `grupo27`.`gen` (G_id_gen, G_cromosoma, G_nombre_gen, G_posicion_gen, G_descripcion_gen)
VALUES 
('TP53', 17, 'Tumor Protein p53', '7668421', 'Guardián del genoma; esencial para regular el ciclo celular y prevenir tumores.'),
('BRCA1', 17, 'BRCA1 DNA Repair Associated', '43044295', 'Implicado en reparación de roturas de doble cadena del ADN y estabilidad genómica.'),
('APOE', 19, 'Apolipoprotein E', '44905791', 'Proteína clave en el metabolismo de lípidos y riesgo genético para el Alzheimer.'),
('INS', 11, 'Insulin', '2159779', 'Hormona pancreática fundamental para regular los niveles de glucosa en sangre.'),
('EGFR', 7, 'Epidermal Growth Factor Receptor', '55019017', 'Receptor de membrana que regula el crecimiento celular; implicado en varios cánceres.'),
('CFTR', 7, 'CF Transmembrane Cond. Regulator', '117465784', 'Canal de cloruro; sus mutaciones causan la fibrosis quística por transporte anómalo.'),
('ACE2', 23, 'Angiotensin Converting Enzyme 2', '15518103', 'Enzima de superficie celular que sirve como punto de entrada para el SARS-CoV-2.'),
('MTHFR', 1, 'Methylenetetrahydrofolate Reductase', '11785730', 'Enzima crítica para procesar folato (vitamina B9) y mantener niveles de homocisteína.'),
('TNF', 6, 'Tumor Necrosis Factor', '31575567', 'Citocina proinflamatoria que regula la respuesta inmune y la apoptosis celular.'),
('MYH7', 14, 'Myosin Heavy Chain 7', '23412739', 'Proteína motora del músculo cardíaco; vinculada a miocardiopatía hipertrófica.');

-- Gestión de errores: Ni La columna del nombre G_nombre_gen ni la descripción del gen G_descripcion_gen pueden ser NULL:
-- INSERT INTO gen (G_id_gen, G_cromosoma, G_posicion_gen)
-- VALUES ('ERROR1', 1, '100');

-- INSERT INTO gen (G_id_gen, G_cromosoma, G_nombre_gen, G_posicion_gen)
-- VALUES ('ERROR1', 1, 'Gen Fallido', '100');




-- -----------------------------------------------------
-- Table `grupo27`.`variante`
-- -----------------------------------------------------

CREATE TABLE IF NOT EXISTS `grupo27`.`variante` (
    v_id_variante VARCHAR(10) PRIMARY KEY,
    v_nomenclatura_c VARCHAR(50) NOT NULL ,
    v_id_gen VARCHAR(15) NOT NULL ,
    v_secuencia INT NOT NULL,
    v_tipo_variante VARCHAR(10) NOT NULL,
    v_alelo_referencia VARCHAR(15) NOT NULL,
    v_alelo_mutado VARCHAR(6) DEFAULT '-',
    v_posicion_cromosomica VARCHAR(50) NOT NULL,
    FOREIGN KEY (`v_id_gen`) REFERENCES `grupo27`.`gen` (`G_id_gen`));

INSERT INTO `grupo27`.`variante` (v_id_variante, v_nomenclatura_c, v_id_gen, v_secuencia, v_tipo_variante, v_alelo_referencia, v_alelo_mutado, v_posicion_cromosomica)
VALUES 
('var1', 'NM_000546.5:c.215C>G', 'TP53', '215', 'SNV', 'C', 'G', 'chr17:7673803'), 
('var4', 'NM_004333.6:c.1799T>A', 'INS',  '1799', 'SNV', 'T', 'A', 'chr7:140453136'), 
('var5', 'NM_005228.5:c.2573T>G', 'EGFR', '2573', 'SNV', 'T', 'G', 'chr7:55181378'), 
('var6', 'NM_000314.8:c.1582G>A', 'CFTR','1582', 'SNV', 'G', 'A', 'chr10:87863126'), 
('var7', 'NM_000518.5:c.20A>T', 'ACE2', '20', 'SNV', 'A', 'T', 'chr11:5227002'), 
('var10', 'NM_000277.2:c.1222C>T', 'MYH7', '1222', 'SNV', 'C', 'T', 'chr12:102840062');

-- Comprobación de que el default de - para el alelo mutado funciona:
INSERT INTO `grupo27`.`variante` (v_id_variante, v_nomenclatura_c, v_id_gen, v_secuencia, v_tipo_variante, v_alelo_referencia, v_posicion_cromosomica)
VALUES 
('var2', 'NM_007294.4:c.68_69del', 'BRCA1', '68', 'DEL', 'AG', 'chr17:43045729'), 
('var3', 'NM_000059.4:c.5946delT', 'APOE', '5946', 'DEL', 'T', 'chr13:32356432'), 
('var8', 'NM_000249.4:c.35delG', 'MTHFR', '35', 'DEL', 'G', 'chr13:20189547'), 
('var9', 'NM_001354689.2:c.5266dupC', 'TNF',  '5266', 'INS', 'C', 'chr17:43070945');




-- -----------------------------------------------------
-- Table `grupo27`.`paciente`
-- -----------------------------------------------------

CREATE TABLE IF NOT EXISTS `grupo27`.`paciente` (
    `P_DNI` VARCHAR(50) PRIMARY KEY,
    `P_Nombre` VARCHAR(30) NOT NULL,
    `P_Edad` INT(2) NOT NULL,
    `P_Fecha_diag` DATE NOT NULL,
    `P_ID_variante` VARCHAR(5) NOT NULL,
	FOREIGN KEY (`P_ID_variante`) REFERENCES `grupo27`.`variante` (`v_id_variante`)
) ENGINE=INNODB;

INSERT INTO `grupo27`.`paciente` (`P_DNI`, `P_Nombre`, `P_Edad`, `P_Fecha_diag`, `P_ID_variante`)
VALUES
('99966699Y', 'Leovigildo de Biedma', 55, '2025-02-02', 'var1'),
('71007100H', 'Ambrosio Gómez', 62, '2025-03-03', 'var2'),
('09111250P', 'Eufrasio De la O', 49, '2026-01-08', 'var3'),
('30020011A', 'María Dolores de Barriga', 31, '2025-11-21', 'var4'),
('52525252V', 'Inés Esario', 45, '2025-12-11', 'var5'),
('11223344B', 'Aitor Menta', 21, '2025-09-07', 'var6'),
('22299194W', 'Alan Brito', 80, '2025-06-06', 'var7'),
('66611666B', 'Elba Lazo', 26, '2026-01-08', 'var8'),
('12141621C', 'Aniceto Espidiforo', 65, '2025-01-03', 'var9'),
('90977786H', 'Glácida sinclética', 49, '2025-05-11', 'var10');




-- -----------------------------------------------------
-- Table `grupo27`.`estudio`
-- -----------------------------------------------------

CREATE TABLE IF NOT EXISTS `grupo27`.`estudio` (
	`E_id_gen` VARCHAR(45) NOT NULL,
    `E_id_variante` VARCHAR(10) NOT NULL,
	`E_Referencia_DOI` VARCHAR(8) NOT NULL PRIMARY KEY,
	`E_Fecha_publicacion` INT(8) NOT NULL,
    `E_Revista` VARCHAR(50) NOT NULL,
    `E_Titulo` VARCHAR(150) NOT NULL,
    CONSTRAINT `Formato_referencia_DOI` CHECK (`E_Referencia_DOI` REGEXP '^[a-zA-Z]{4}/[0-9]{3}$'), # cuatro letras, /, tres números
    FOREIGN KEY(`E_id_gen`) REFERENCES `grupo27`.`gen` (`G_id_gen`),
    FOREIGN KEY(`E_id_variante`) REFERENCES `grupo27`.`variante` (`v_id_variante`))
    ENGINE=INNODB;
    
INSERT INTO `grupo27`.`estudio` (E_id_gen, E_id_variante, E_Referencia_DOI, E_Fecha_publicacion, E_Revista, E_Titulo )
VALUES
('TP53', 'var1', 'ABCD/123', 07072003, 'Nature', 'TP53 role in cell homeostasis'),
('BRCA1', 'var2', 'BRCA/001', 15102022, 'Science', 'DNA repair mechanisms and BRCA1 mutations'),
('APOE', 'var3', 'APOE/555', 20012023, 'The Cell', 'Lipid metabolism and Alzheimer risk factors'),
('INS', 'var4', 'INSU/777', 12052021, 'Diabetes Care', 'Insulin gene variants in early-onset diabetes'),
('EGFR', 'var5', 'CANC/999', 30082024, 'Cell', 'Targeted therapies for EGFR mutant tumors'),
('CFTR', 'var6', 'CFTR/101', 05112020, 'ROSaL', 'Chloride channel function in cystic fibrosis'),
('ACE2', 'var7', 'SARS/202', 22032022, 'Virology', 'ACE2 receptor binding and viral entry'),
('MTHFR', 'var8', 'FOLA/333', 14022023, 'Nutrients', 'MTHFR enzymatic activity and folate processing'),
('TNF', 'var9', 'INFL/444', 09092021, 'Immunology', 'Cytokine response in chronic inflammation'),
('MYH7', 'var10', 'MYOH/888', 11122024, 'Corazon', 'Myosin heavy chain mutations in heart disease');

-- -----------------------------------------------------
-- Table `grupo27`.`anotacion`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `grupo27`.`anotacion` (
   `A_id_anotacion` VARCHAR(45) PRIMARY KEY,
    `A_función_gen` TEXT NOT NULL,
    `A_tipo` VARCHAR(100) NOT NULL,
    `A_id_gen` VARCHAR(45) NOT NULL,
    `A_id_variante` VARCHAR(10) NULL, 
    FOREIGN KEY (`A_id_gen`) REFERENCES `grupo27`.`gen` (`G_id_gen`),
    FOREIGN KEY (`A_id_variante`) REFERENCES `grupo27`.`variante` (`v_id_variante`)) 
    ENGINE=INNODB;
    
INSERT INTO `grupo27`.`anotacion` (A_id_anotacion, A_función_gen, A_tipo, A_id_gen, A_id_variante)
VALUES
('ANOT001', 'Mantenimiento de la integridad genómica mediante la parada del ciclo celular.', 'Funcional', 'TP53', NULL),
('ANOT002', 'Variante patogénica vinculada a un aumento del riesgo de tumores en edad temprana.', 'Clínica', 'TP53', 'var1'),
('ANOT003', 'Reparación de roturas de doble cadena en el ADN por recombinación homóloga.', 'Funcional', 'BRCA1', NULL),
('ANOT004', 'Variante clasificada como de alta penetrancia para cáncer de mama hereditario.', 'Clínica', 'BRCA1', 'var2'),
('ANOT005', 'Transporte de colesterol y otras grasas en el torrente sanguíneo.', 'Metabólica', 'APOE', NULL),
('ANOT006', 'Alelo asociado con la acumulación de placas de amiloide en el tejido cerebral.', 'Clínica', 'APOE', 'var3'),
('ANOT007', 'Codificación de la hormona responsable de la absorción de glucosa.', 'Fisiológica', 'INS', 'var4'),
('ANOT008', 'Implicado en la señalización celular para la proliferación y diferenciación.', 'Funcional', 'EGFR', 'var5'),
('ANOT009', 'Receptor crítico para la entrada de virus respiratorios en la célula huésped.', 'Infecciosa', 'ACE2', 'var7'),
('ANOT010', 'Proteína esencial para la contracción del sarcómero en el músculo cardíaco.', 'Estructural', 'MYH7', NULL);




-- -----------------------------------------------------
-- Table `grupo27`.`secuencia_canonica` 
-- Contiene las columnas para poder formar el archivo FASTA si se quisiera exportar la información por fila
-- -----------------------------------------------------

CREATE TABLE IF NOT EXISTS `grupo27`.`secuencia_canonica` (
    `S_id_gen` VARCHAR(45) NOT NULL,
    `S_genoma_REF` VARCHAR(7) NOT NULL,
    `S_tipo` VARCHAR(45) NOT NULL,
    `S_posicion_relativa` VARCHAR(45) NOT NULL,
    `S_secuencia_ADN`VARCHAR(1000) NOT NULL,
    PRIMARY KEY (`S_id_gen`, `S_genoma_REF`, `S_tipo`), -- la clave primaria es la fusión entre el ID del gen, el genoma de referencia y tipo
    FOREIGN KEY (`S_id_gen`) REFERENCES `grupo27`.`gen` (`G_id_gen`)
)  ENGINE=INNODB;

-- En la columan S_genoma_REF limitamos el formato de entrada para que coincida con "GRCh00"
-- Usamos REGEXP para asegurar que en el inicio (^) aparezcan las letras "GRCh" seguido de dos numeros y final ($)
ALTER TABLE `grupo27`.`secuencia_canonica` 
ADD CONSTRAINT `formato_genoma_referencia` 
CHECK (`S_genoma_REF` REGEXP '^GRCh[0-9]{2}$');

-- En la columan S_secuencia_ADN solo admite caracteres A, G, T, C 
-- Usamos REGEXP para asegurar que desde el inicio (^) hasta el fin ($) solo haya AGTC
ALTER TABLE `grupo27`.`secuencia_canonica` 
ADD CONSTRAINT `solo_bases_nitrogenadas` 
CHECK (`S_secuencia_ADN` REGEXP '^[AGTCagtc]+$');

-- En la columan S_secuencia_ADN controlamos la longitud del registro: mínima 10nt y máxima 1000nt
-- Usamos CHAR_LENGTH para contar los caracteres
ALTER TABLE `grupo27`.`secuencia_canonica` 
ADD CONSTRAINT `longitud_secuencia` 
CHECK (CHAR_LENGTH(`S_secuencia_ADN`) BETWEEN 10 AND 1000);


-- Inserción de datos en `secuencia_canonica`
INSERT INTO `grupo27`.`secuencia_canonica` (S_id_gen, S_genoma_REF, S_tipo, S_posicion_relativa, S_secuencia_ADN)
VALUES 
('TP53', 'GRCh37', 'Exón 1', 'chr17:7571720-7590863', 'ACTGCCATGGAGGAGCCGCAGTCAGATCCTAGCGTCGAGCCCCCTCTGAGTCAGGAAACATTTTCAGACCTATGGAAACTACTTCCTGAAAACAACGTTCTGTCCCCCTTGCCGTCCCAAGCAATTGGATGATTTGATGCTGTCCCCGGACGATATTGAACCAATGGTTCACTGAAGACCCAGGTCCAGATGAAGCTC'),
('TP53', 'GRCh38', 'Exón 1', 'chr17:7661779-7687538', 'ACTGCCATGGAGGAGCCGCAGTCAGATCCTAGCGTCGAGCCCCCTCTGAGTCAGGAAACATTTTCAGACCTATGGAAACTACTTCCTGAAAACAACGTTCTGTCCCCCTTGCCGTCCCAAGCAACTGGATGATTTGATGCTGTCCCCGGACGATATTGAACTAATGGTTCACTGAAGACCCAGGTCCAGATGAAGCTC'),
('BRCA1', 'GRCh37', 'Intrón 2', 'chr17:41196312-41277500', 'GGTGGCCACCGCCACCCCCACCCGGCAGCCAGCATGATGCCCCGAGGCCGCCGCCTCTTCGCTGTCTTCCTGGCCTGCATCCTGGCCCTGGCCCTGGCCCTGACCAACGGCAGCAGCGTCTTCCTGGCCTGCATCCTGGCCCTGGCCCTGGGGGGTTTTTTTTTTAATATAGATCGCGTAGACCCCTGACCAACGGCAGCAGCGTCTTCCTGGCCTGCATCCTGGC'),
('APOE', 'GRCh37', 'CDS', 'chr19:44905791-44909393', 'GGTGCAGGAAGAGCTGCAGGCGGGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGC'),
('INS', 'GRCh37', 'Promotor', 'chr11:2181009-2182430', 'AGCCCTCCAGGACAGCCTTTTCCTCTAACGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCCTGTGGTCCTCACTTCCATTGCTGTCCTTGCCGTCCCAAGCAATGGATGATTTGATGCTGTGCGCGGCGGGCGCGCGGGGCGCGCGGCGCGCGGCCCCGGACGATATTGAACAATGGTTCACTGAAGA'),
('TNF', 'GRCh37', '5\' UTR', 'chr6:31543339-31546114', 'CAGGCGGTGCTTGTTCCTCAGCCTCTTCTCCTTCCTGATCGTGGCAGGCGCCACCACGCTCTTCTGCCTGCTGCACTTTGGAGTGATCGGCCCCCAGAGGGAAGAGTCCCCCAGGGACCTCTCTCTAATCAGC'),
('ACE2', 'GRCh37', 'Exón 1', 'chrX:15579156-15579540', 'ATGTCAAGCTCTTCCTGGCTCCTTCTCAGCCTTGTTGCTGTAACTGCTGCTCAGTCCACCATTGAGGAACAGGCCAAGACATTTTTGGACAAGTTTAACCACGAAGCCGAAGACCTGTTCTATCAAAGTTCACTTGCTTCTTGGAATTATAACACCAATATTACTGAAGGACAAGAGCAAACGGTTGAACACAATTCTAAATACAATGAGCACCATCTACAGTACTGGAAAAGTTTGTAACCCAGATAATCCACAAGAATGCTTATTACTTGAACCAGGTTTGAA'),
('APOE', 'GRCh38', '5\' UTR', 'chr19:44905750-44905850', 'GCTGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGCGCGCAGCCCGGC'),
('BRCA1', 'GRCh37', 'Intrón 1', 'chr17:43125200-43125400', 'GTAGTAAATTTTAGTTTAATTCATTTTTCAGTGATGTTCCATTTTTTAAAGCATTAAAAGAGACTTGTGTACATGTACATTTGTGCATATATGTTTTTTTTTTAATGTAGAGTTTCTTGTTTTAGGTGGAGAGACTTGTGTACATGTACATTTGTGCATATATGTTTTTTTTTTAATGTAGAGTTTCTTGTTTT'),
('CFTR', 'GRCh38', 'Exón 1', 'chr7:117465784-117466000', 'AATTGGAAGCAAATGACATCACAGCAGGTCAGAGAAAAAGGGTTGAGCGGCAGGCACCCAGAGTAGTAGGTCTTTGGCATTAGGAGCTTGAGCCCAGACGGCCCTAGCAGGGACCCCAGCGCCCGAGAGACCATGCAGAGGTCGCCTCTGGAAAAGGCCAGCGTTGTCTCCAAACTTTTTTTCAGCTGGACCAGACCAATTTTGAGGAAAGGATACAGACAGCGCCTGGAATTGTCAGACATATACCAAATCCCTTCTGTTGATTCTGCTGACAATCTATCTGAAAAAATTGGAAAGCAAATGACATCACAGCAGGTCAGAGAAAAAGGGTTGAGCGGCAGGCACCCAGAGTAGTAGGTCTTTGGCATTAGGAGCTTGAGCCCA'),
('EGFR', 'GRCh38', 'Promotor', 'chr7:55086000-55086500', 'CCCCCGGCGCAGCGCGGCCGCAGCAGCCTCCGCCCCCCGCACGGTGTGAGCGCCCGCCGCGGCCGAGGCGGCCGGAGTCCCGAGCTAGCCCCGGCGGCCGCCGCCGCCCAGACCGGACGACAGGCCACCTCGTCGGCGTCCGCCCGAGTCCCCGCCTCGCCGCCAACGCCACAACCACCGCGCACGGCCCCCTGACTCCGTCCAGTATTGATTCCTTCTTAAAGACCATCCAGGAGGTGGCT'),
('INS', 'GRCh37', 'CDS', 'chr11:2159779-2161000', 'AGCCCTCCAGGACAGCCTTTTCCTCTAACCCTGTGGTCCTCACTTCCATTGCTGTCCTTGCCGTCCCAAGCAATGGATGATTTGATGCTGTCCCCGGACGATATTGAACAATGGTTCACTGAAGACCCAGGTCCAGATGAAGCTCCAGAATTGATGCTGTCCCCGGACGATATTGAACAATGGTTCACTGAAGA'),
('MTHFR', 'GRCh37', 'Exón 5', 'chr1:11785723-11785900', 'GCTTTGAGGCTGACCTGAAGCACTTGCTGGACCTGGTGAGGGAGCTGAACCTGCTGCAGCGGCTGGAGGCCGACCTGAGGGAGCTGAACCTGCTGCAGCGGCTGGAGGCCGACCTGAGGGAGCTGAACCTGCTGCAGCGGCTGGAGGCCGACCTGAGGGAGCTGAACCTGCTGCAGCGGCTGGAGGCCGACCT'),
('MYH7', 'GRCh38', 'Exón 3', 'chr14:23412739-23413000', 'ACTGATGAGATGGAGCAGATCAAGAACCTGTCCACCTTCCTGGAGCTGAAGCAGATCAAGAACCTGTCCACCTTCCTGGAGCTGAAGCAGATCAAGAACCTGTCCACCTTCCTGGAGCTGAAGCAGATCAAGAACCTGTCCACCTTCCTGGAGCTGAAGCAGATCAAGAACCTGTCCACCTTCCTGGAGCTG'),
('TNF', 'GRCh38', '3\' UTR', 'chr6:31575567-31575800', 'CAGGCGGTGCTTGTTCCTCAGCCTCTTCTCCTTCCTGATCGTGGCAGGCGCCACCACGCTCTTCTGCCTGCTGCACTTTGGAGTGATCGGCCCCCAGAGGGAAGAGTCCCCCAGGGACCTCTCTCTAATCAGCCCTCTGGCCCAGGCAGTCAGATCATCTTCTCGAACCCCGAGTGACAAGCCTGTAGCCCAT'),
('TP53', 'GRCh37', 'Exón 11', 'chr17:7668421-7668600', 'ACTGCCATGGAGGAGCCGCAGTCAGATCCTAGCGTCGAGCCCCCTCTGAGTCAGGAAACATTTTCAGACCTATGGAAACTACTTCCTGAAAACAACGTTCTGTCCCCCTTGCCGTCCCAAGCAATGGATGATTTGATGCTGTCCCCGGACGATATTGAACAATGGTTCACTGAAGACCCAGGTCCAGATGAAGCTC');
