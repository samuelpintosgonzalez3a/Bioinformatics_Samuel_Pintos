# -*- coding: utf-8 -*-

#%%
#1A. conda create --name actividad1 pandas matplotlib seaborn spyder
#1B. conda activate actividad1
#    conda update spyder
#1C. conda env list
#    conda list
#%%
#2A. TITLE  TUMOR SUPPRESSOR P53 COMPLEXED WITH DNA
#    AUTHOR Y.CHO,S.GORINA,P.D.JEFFREY,N.P.PAVLETICH


#%%
#2B.

file_path = r"C:/Users/samue/Documents/PYTHON/1tup.pdb"

aminoacidos_p53 = []  #Creo una lista con los aá

with open(file_path, "r") as pdb_file:
    for linea in pdb_file:
        if linea.startswith("SEQRES"): #filtro lo que empieza por SEQRES
            letra_cadena = linea[11:12].strip() #Aquí aparece la letra de la cadena
            if letra_cadena in ["A", "B", "C"]:
                residuos = linea[19:].split() #aá están a partir de la línea 20
                for res in residuos: #me aseguro de coger aá que es lo único que tiene 3 letras
                    if len(res) == 3:
                        aminoacidos_p53.append(res)

print(f"Se han extraído {len(aminoacidos_p53)} aminoácidos.")

# SE HAN EXTRAÍDO 657 AMINOÁCIDOS.
#%%
#2C


conteo_aminoacidos = {}

for aa in aminoacidos_p53:
    if aa in conteo_aminoacidos:
        # Si el aminoácido ya está en el diccionario, incrementamos su contador
        conteo_aminoacidos[aa] += 1
    else:
        # Si es la primera vez que aparece, creamos la clave con valor 1
        conteo_aminoacidos[aa] = 1

print("Conteo de aminoácidos en la P53:")
for aa, cantidad in conteo_aminoacidos.items():
    print(f"{aa}: {cantidad}")
# SER: 60    VAL: 45    PRO: 54    GLN: 21
#LYS: 24     THR: 48    TYR: 24    GLY: 45
#PHE: 15     ARG: 57    LEU: 48    HIS: 27
#ALA: 24     CYS: 30    ASN: 33    MET: 18
#TRP: 3      ASP: 24    ILE: 18    GLU: 39
#%%
#2D

import matplotlib.pyplot as plt
import seaborn as sns

nombres_aa = list(conteo_aminoacidos.keys())
valores_aa = list(conteo_aminoacidos.values())


plt.figure(figsize=(12, 6))      # Ajustamos el tamaño para que se lean bien las etiquetas


# 'palette' asigna colores distintos automáticamente; 'hue' evita avisos de versiones recientes
grafico = sns.barplot(x=nombres_aa, y=valores_aa, hue=nombres_aa, palette="viridis", legend=False)

# 3. Añadir títulos y etiquetas
plt.title("Frecuencia de cada aminoácido en la secuencia de la P53", fontsize=15)
plt.xlabel("Tipo de Aminoácido", fontsize=12)
plt.ylabel("Número de apariciones", fontsize=12)

plt.savefig("frecuencia_aminoacidos.png", dpi=300)
plt.show()

#%%
#3A.

import pandas as pd

# Definimos los nuevos nombres de las columnas
nuevos_nombres = ['id', 'dieta', 'pulsaciones', 'tiempo', 'actividad']

# Leemos el archivo aplicando el delimitador y los nuevos nombres
df = pd.read_csv('actividad.csv', sep=';', names=nuevos_nombres, header=0)

# Verificamos el cambio
print(df.head())

#%%
#3B

# 1. Determinar la presencia de celdas vacías (conteo por columna)
print("Valores nulos por columna antes:")
print(df.isnull().sum())

# 2. Eliminar las filas que contengan al menos un valor nulo
# El parámetro inplace=True aplica los cambios directamente sobre el DataFrame original
df.dropna(inplace=True)

print("\nValores nulos después de la limpieza:")
print(df.isnull().sum())

#Valores nulos por columna antes:
#id             0
#dieta          0
#pulsaciones    0
#tiempo         0
#actividad      0
#dtype: int64

#Valores nulos después de la limpieza:
#id             0
#dieta          0
#pulsaciones    0
#tiempo         0
#actividad      0
#dtype: int64
#%%
#3C

frecuencia_dieta = df['dieta'].value_counts()

print(frecuencia_dieta)

#dieta
#low fat    45
#no fat     45

#2 niveñes y 45 de frecuencia en cada uno
#%%
#3D


agrupado = df.groupby('actividad') #Agrupando por la actividad solo tendremos 3 categorías
lista_agrupada = list(agrupado)

# Verificamos la longitud de la lista
print(f"La lista tiene {len(lista_agrupada)} elementos.")
print(lista_agrupada)

#Estos elementos corresponden a rest, running y walking, 
#ya que se agrupan en función de la actividad. 
#Cada elemento es una tupla (índice 0, 1 y 2)
#%%
#3E.
estadisticas_pulsaciones = df.groupby('actividad')['pulsaciones'].agg(['mean', 'std'])

print(estadisticas_pulsaciones)
#%%
#3F.
df_ciudades = pd.read_csv('ciudades.tsv', sep='\t')
df_completo = pd.merge(df, df_ciudades, on='id')
print(df_completo)

#%%
#3G.

import seaborn as sns
import matplotlib.pyplot as plt

df['tiempo'] = pd.Categorical(df['tiempo'], categories=['1 min', '15 min', '30 min'], ordered=True)

# 2. Creamos la figura multi-facetada con relplot
g = sns.relplot(
    data=df,
    x='tiempo',
    y='pulsaciones',
    hue='dieta',
    col='actividad', # faceta por cada tipo de actividad
    kind='line',    
    marker='o',
    height=4,
    aspect=0.8
)

# 3. Personalización
g.fig.suptitle('Evolución de Pulsaciones por Tiempo, Actividad y Dieta', y=1.08, fontsize=14)
g.set_axis_labels('Tiempo transcurrido', 'Pulsaciones (bpm)')
g.set_titles("Actividad: {col_name}")

plt.show()
#%% 







