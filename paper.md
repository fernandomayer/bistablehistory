---
title: 'bistablehistory: an R package for history-dependent analysis of perceptual time series'
tags:
  - R
  - vision
  - time series
  - binocular riavlry
authors:
  - name: Alexander Pastukhov
    orcid: 0000-0002-8738-8591
    affiliation: "1, 2"
affiliations:
  - name: Department of General Psychology and Methodology, University of Bamberg, Bamberg, Bavaria, Germany.
    index: 1
  - name: Forschungsgruppe EPÆG (Ergonomics, Psychological Æsthetics, Gestalt), Bamberg, Bavaria, Germany.
    index: 2
date: 01 October 2021
bibliography: paper.bib
---

# Summary
Our perception is a subject to a process of adaptation that changes its operating properties [@Clifford2007]. This process manifests itself in a plethora of perceptual illusions and so-called aftereffects. It also plays an important role during perception of multistable stimuli, such as a Necker cube (Fig. 1A). These are compatible with several, typically two, comparably likely perceptual interpretations. In case of the Necker cube, one can perceive as "upwards" or "downwards" and during continuous viewing the perception switches between these alternatives (Fig. 1B).

![Perception of Necker cube stimulus. A) Necker cube. B) Schematic time series of perceptual alternations. C) Example distribution of perceptual dominance phases' duration fitting with Gamma distribution. \label{fig:fig1}](fig1.png){width=80%}

Typically, time series for such multistable stimuli are fitted using Gamma distribution (Fig. 1C). However, this approach ignores the effect prior perception through adaptation [@VanEe2009] and violates assumptions about independent and identically distributed samples. The `bistablehistory` package solves this problem by accounting for the slow accumulation process via a homogeneous first order process [@PastukhovBraun2011], providing tools for fitting time series using various distributions. It also allows for fitting while accounting for additional random or fixed factors. In addition, it provides a tool for extracting the estimated accumulated adaptation or for computing it directly for further usage. The aim of the package is to streamline the analysis of time series for perceptual multistability and experiments on continuous perception in general.

The package is built using Stan probabilistic programming language [@carpenter2017stan]. Thus, it provides posterior distributions, ability to compare models via information criteria [@loo], etc. In addition, the package provides Stan code for performing the estimation and an example that explains how to implement a custom Stan model that relies on it. The source code for `bistablehistory` has been archived to Zenodo with the linked DOI: [@zenodo]

# References
