# AI Workflow Builder

An interactive visual interface for research administrators to build and compose AI workflows using a LEGO-like brick metaphor.

## Overview

The AI Workflow Builder simplifies complex research administration tasks by breaking them into three fundamental types of AI operations. Users assemble these operations like LEGO bricks to create complete workflows, with intelligent semantic matching automatically categorizing each step.

## Three Types of AI Tasks

The system organizes AI operations into three distinct categories:

### 1. **Extraction** (Blue Bricks)
Data retrieval and gathering operations that pull information from existing sources.
- *Examples:* Extract grant data, retrieve compliance documents, gather proposal references, fetch financial records
- *Purpose:* Identify and locate relevant information needed for downstream processing

### 2. **Transformation/Analysis** (Green Bricks)
Processing, analysis, and refinement operations that work with extracted data.
- *Examples:* Analyze requirements, process documents, synthesize information, evaluate proposals, transform data formats
- *Purpose:* Convert raw data into meaningful insights and structured information

### 3. **Formatting** (Yellow Bricks)
Document generation and output operations that produce final deliverables.
- *Examples:* Generate reports, format documents, export summaries, create proposals, compile outputs
- *Purpose:* Produce polished, ready-to-use documents from processed information

## Building Workflows

**Task Bricks** (right panel) contain the available operations ready to be used. **Workflow Tower** (left panel) shows your assembled workflow.

1. **Describe a Task**: Click any brick in the Task Bricks panel and enter a description of what you want it to do
2. **See the Category**: The app automatically detects the task type and colors the brick accordingly using semantic AI matching
3. **Drag to Workflow**: Drag colored bricks from the Task Bricks pool into the Workflow Tower to build your sequence
4. **Execute in Order**: Bricks stack from bottom to top, executing in sequence—each step builds on the previous output

The semantic matching system uses GloVe embeddings trained on research administration vocabulary, achieving 95.9% accuracy in task categorization.

## Getting Started

### Development

To run the app locally:

```r
shiny::runApp("app")
```

### Deployment

To export the app as a static shinylive site:

```r
shinylive::export("app", "docs")
```

This will create a static version of the app in the `docs` folder that can be deployed to GitHub Pages or any static hosting service.
