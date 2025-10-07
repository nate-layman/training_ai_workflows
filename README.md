# AI Workflow Builder

An interactive Shiny application for building AI workflows using a visual brick-based interface.

## Development

To run the app locally:

```r
shiny::runApp("app")
```

## Building for Shinylive

To export the app as a static shinylive site:

```r
shinylive::export("app", "docs")
```

This will create a static version of the app in the `docs` folder that can be deployed to GitHub Pages or any static hosting service.
