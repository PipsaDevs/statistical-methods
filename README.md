# statistical-methods
Yet another course at the ETSII in the UMA

hello teacher

> @inteltank1, saludando a nuestro querido profesor de Métodos estadisticos

## Wadaheil is this
Basicamente este es el repositorio donde metemos todas las funciones que vamos viendo por las diapositivas del temario y no están implementadas nativamente en R.

## Estructura del paquete
Esto es un paquete de R, con lo cual, todos los archivos van a estar metidos en R/, y en función si la fórmula es de un capitulo u otro, pues está dentro de un archivo u otro.

Cada función que se define está acompañada con sus comentarios para que roxygendoc pueda generar documentación automaticamente (esto no hace falta que lo hagas, ya lo hace el pipeline automaticamente)

Tambien tiene un generador de documentación que funciona con roxygenv2, para consultar las funciones en el navegador en vez de Rstudio to chiquito.

## ¿Como usarlo de cara a los examanes?

### Contenido
Cada vez que se hace un commit o se hace un merge a la rama main, se crea un artefacto, que contendrá todo lo que usarás para los examenes.

Este artefacto contiene:
- El codigo fuente del paquete
- Paquete compilado en .zip y .tar.gz
- documentation.html, la pagina estática que contiene la documentación de todas las funciones
- load_all_fallback.R, un script de emergencia por si no puedes instalar el paquete en RStudio

### Antes del examen
Comprobaciones iniciales:
- Comprueba que puedes usar el paquete compilado en tu casa con RStudio
- Mira que el código que haya en main sea el correcto, y que todas las funciones estén documentadas

Pasos a seguir:
- Descarga el artefacto generado por la última ejecución del workflow
- Descomprime el zip en tu USB

### En el examen
- A través de Tools/Install new packages..., selecciona el .zip o el .tar.gz que hay en el pendrive
- Si no se instala, ejecuta el script load_all_fallback.R
- Haz uso de todas las funciones que quieras
- IMPORTANTE: Antes de acabar el examen, usa la función bundle() por cada función de la libreria que uses en la consola para generar código que no dependa de otras cosas del paquete
- Elimina las lineas que contengan library(statmethods)
