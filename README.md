# statistical-methods

[![Build Pendrive Deliverables](https://github.com/PipsaDevs/statistical-methods/actions/workflows/build-pendrive.yml/badge.svg)](https://github.com/PipsaDevs/statistical-methods/actions/workflows/build-pendrive.yml)
[![Download latest artifact](https://img.shields.io/badge/⬇️%20Descargar%20artefacto-pendrive__ready-blue?style=flat-square)](https://nightly.link/PipsaDevs/statistical-methods/workflows/build-pendrive/main/pendrive_ready.zip)

---

Yet another course at the ETSII in the UMA

hello teacher

> @inteltank1, saludando a nuestro querido profesor de Métodos Estadísticos

## Wadaheil is this
Básicamente este es el repositorio donde metemos todas las funciones que vamos viendo por las diapositivas del temario y no están implementadas nativamente en R.

## Estructura del paquete
Esto es un paquete de R, con lo cual, todos los archivos van a estar metidos en R/, y en función si la fórmula es de un capítulo u otro, pues está dentro de un archivo u otro.

Cada función que se define está acompañada con sus comentarios para que roxygen2 pueda generar documentación automáticamente (esto no hace falta que lo hagas, ya lo hace el pipeline automáticamente)

También tiene un generador de documentación que funciona con roxygen2, para consultar las funciones en el navegador en vez de Rstudio to chiquito.

## ¿Cómo usarlo de cara a los exámenes?

### Contenido
Cada vez que se hace un commit o se hace un merge a la rama main, se crea un artefacto, que contendrá todo lo que usarás para los exámenes.

Este artefacto contiene:
- El código fuente del paquete
- Paquete compilado en .zip y .tar.gz
- documentation.html, la página estática que contiene la documentación de todas las funciones
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
- IMPORTANTE: Antes de acabar el examen, usa la función bundle() por cada función de la librería que uses en la consola para generar código que no dependa de otras cosas del paquete
- Elimina las lineas que contengan `library(statmethods)`
