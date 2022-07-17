
# Table of Contents

1.  [Goals](#orge02dd06)
2.  [Status and roadmap](#orgbcbccc0)
3.  [Getting started](#org4f82322)
    1.  [Installing](#org251e6dd)
        1.  [straight.el](#orgf0626ca)
    2.  [Configuration](#orga439e64)
        1.  [Example configuration](#orgfc37036)
    3.  [Interactive functions](#org7d50894)
4.  [Notes structure](#org6f124d9)
    1.  [Appellation/region](#org94b6b05)
    2.  [Producer](#org6db2c10)
    3.  [Grape](#orgaf95b47)
    4.  [Vino entry](#orgdd57a7e)
    5.  [Vino rating](#org1d40bf1)
    6.  [Tags](#org928c38d)
5.  [Database](#orgc6feea6)
6.  [How to &#x2026;](#org4bae89b)
    1.  [Configure templates](#org997a129)
    2.  [Configure rating system](#org726fda1)
    3.  [Store images and other attachments](#org6d02485)
    4.  [Query additional metadata when creating a new wine entry](#orgdcfaa21)
    5.  [Query additional metadata when rating a wine entry](#org46d9b0b)
7.  [Coding](#org2667c8a)
8.  [Building and testing](#org4bf21c4)
9.  [FAQ](#org326bb36)
    1.  [Why not generalise?](#orgbeae03e)
    2.  [Why not Cellar Tracker, Vivino, etc.?](#org215c4f7)
10. [Acknowledgements](#org0caadc1)

<h1 align="center">Vino</h1>
<p align="center">
  <img width="256px" src="images/logo.png" alt="Banner">
</p>
<h3 align="center">🍷It's your cellar, your dear cantina 🍷</h3>
<p align="center">
  <a href="https://github.com/d12frosted/vino/releases">
    <img alt="GitHub tag (latest by date)" src="https://img.shields.io/github/v/tag/d12frosted/vino">
  </a>
  <a href="https://github.com/d12frosted/vino/actions?query=workflow%3ACI">
    <img src="https://github.com/d12frosted/vino/workflows/CI/badge.svg" alt="CI Status Badge">
  </a>
</p>

`vino` is an Emacs program for cellar tracking and wine rating. Every entity is
stored as separate Org file, `vino` provides convenient functions to query and
manipulate these files.

Please note, that `vino` does not ship with collection of Org file for wines,
producers, grapes, appellations or wine regions. The idea is that each user
creates their own collection and learns about huge world of wine in the
process.

Checkout [barberry.io](https://barberry.io/) as a showcase of what can be built on top of `vino` together with `vulpea`.


<a id="orge02dd06"></a>

# Goals

-   Provide functionality to create wine entries and to rate them.
-   Provide functionality to track availability of wine entries.
-   Provide functionality to evolve rating system.
-   Provide functionality for maintenance.
-   Provide functionality to query wine entries based on different predicates.
-   Be able to handle big (1000+) collection of wine entries.


<a id="orgbcbccc0"></a>

# Status and roadmap

This project started as a rewrite of [+org-wine.el](https://github.com/d12frosted/environment/blob/3d387cb95353cfe79826d24abbfd1b6091669957/emacs/lisp/%2Borg-wine.el) from [org-brain](https://github.com/Kungsgeten/org-brain) to [org-roam](https://github.com/org-roam/org-roam/) (of
course, using [vulpea](https://github.com/d12frosted/vulpea)) in performance pursuit (it become critical as I have 1k+
of wine entries and around 2k of grape, producers, appellations, regions notes).

-   [X] [v0.1](https://github.com/d12frosted/vino/milestone/1) - feature parity with [+org-wine.el](https://github.com/d12frosted/environment/blob/3d387cb95353cfe79826d24abbfd1b6091669957/emacs/lisp/%2Borg-wine.el).
-   [X] [v0.2](https://github.com/d12frosted/vino/milestone/2) - dedicated database for `vino` needs.
-   [X] v0.3 - migration to `org-roam` V2.
-   [ ] [v0.4](https://github.com/d12frosted/vino/milestone/4) - querying and viewing.
-   [ ] [v0.5](https://github.com/d12frosted/vino/milestone/3) - inventory solutions (built-in and external like [lib-inventory.el](https://github.com/d12frosted/environment/blob/0b5bc480758fd7ceeebc513317732f6337744126/emacs/lisp/lib-inventory.el)).


<a id="org4f82322"></a>

# Getting started


<a id="org251e6dd"></a>

## Installing

`vino` is not available via MELPA, so you can install it manually, using
[straight](https://github.com/raxod502/straight), [quelpa](https://github.com/quelpa/quelpa), or any other tool. Once installed, you need to call
`vino-setup` once.

Keep in mind that `vino` depends on [vulpea](https://github.com/d12frosted/vulpea).


<a id="orgf0626ca"></a>

### straight.el

    (straight-use-package vulpea)
    (vulpea-setup)
    
    (straight-use-package
     '(vino :type git :host github :repo "d12frosted/vino"))
    (vino-setup)
    ;; optionally sync the database
    (vino-db-sync)

In case you have [integration](https://github.com/raxod502/straight.el/#integration-with-use-package) with [use-package](https://github.com/jwiegley/use-package):

    (use-package vino
      :straight (vino
                 :type git
                 :host github
                 :repo "d12frosted/vino")
      :hook
      (after-init . vino-setup)
      :config
      ;; optionally sync the database when vino is loaded
      (vino-db-sync))


<a id="orga439e64"></a>

## Configuration

There are several things that you must configure before using `vino`:

-   `org-roam-directory` - this is where your `vino` files will be located. See
    **Notes structure** for more information on how to structure this directory.
-   `vino-rating-props` - this variable describes your rating system. See
    **Configure rating system** for more information on how to setup this variable.
-   `vino-availability-fn` - function to check availability of `vino-entry`. It is
    called with `ID` of `vino-entry` and must return a cons of acquired and
    consumed numbers, e.g. `(const 10 4)` means that this particular wine was
    acquired 10 times and consumed 4 times, making 6 available.
-   `vino-availability-add-fn` - function to add certain amount of `vino-entry` to
    your cellar. Function is called with `ID` of `vino-entry`, amount, source and
    date arguments.
-   `vino-availability-sub-fn` - function to subtract certain amount of
    `vino-entry` from your cellar. Function is called with `ID` of `vino-entry`,
    amount, action and date arguments.
-   `vino-sources-fn` - function to list wine sources. Used in, for example,
    completion of sources when you acquire a wine. Function is called with `ID` of
    `vino-entry`.

Optionally you can configure the following variables:

-   `vino-db-location` - the full path to file where `vino` database is stored.
-   `vino-db-gc-threshold` - the temporary value for `gc-cons-threshold` to use
    during heavy operations like `vino-db-sync`. For performance considerations,
    you might set it to some high value (like `most-positive-fixnum`).
-   `vino-carbonation-types` - list of carbonation types, e.g. `still` and
    `sparkling`. Modify it in case you want to be more granular, e.g.
    `traditional-sparkling`, `pet-nat`, etc.
-   `vino-colour-types` - list of wine colours, e.g. `red`, `white` and `rose`.
    You might want to add `orange` unless you are marking them as `white`.
-   `vino-sweetness-levels` - a property list where key is carbonation type and
    value is list of sweetness levels (e.g. `dry`, `sweet`, etc).
-   `vino-rating-template` - template for a new wine ratings. See **template
    configuration** section for more information.
-   `vino-rating-extra-meta` - a list of additional meta to be queried when
    rating a wine. See it's documentation for more information.
-   `vino-entry-template` - template a for new wine entry. See **template
    configuration** section for more information.
-   `vino-grape-template` - template for a new grape note. Useful when selecting a
    grape for wine entry that does not exist or when calling `vino-grape-create`.
    See **template configuration** section for more information.
-   `vino-producer-template` - template for a new producer note. Used by
    `vino-producer-create`. See **template configuration** section for more
    information.
-   `vino-region-template` - template for a new region note. Used by
    `vino-region-create`. See **template configuration** section for more
    information.
-   `vino-appellation-template` - template for a new appellation note. Used by
    `vino-appellation-create`. See **template configuration** section for more
    information.


<a id="orgfc37036"></a>

### Example configuration

Mandatory values (uses functions from [+inventory.el](https://github.com/d12frosted/environment/blob/4164a5abd43d478fd314bb299ea4e1024d89c39c/emacs/lisp/+inventory.el)).

    (setq
     org-roam-directory (concat (getenv "HOME")
                                "/org-roam/")
    
     vino-rating-props '((1 . (("SCORE" . 3)))
                         (2 . (("AROMA_QUALITY" . 3)
                               ("AROMA_INTENSITY" . 2)
                               ("AROMA_COMPLEXITY" . 3)
                               ("BALANCE" . 3)
                               ("FLAVOURS" . 2)
                               ("AFTERTASTE" . 3)
                               ("GENERAL" . 4))))
    
     vino-availability-fn
     (lambda (id)
       (cons
        (inventory-total-in wine-inventory-file id)
        (inventory-total-out wine-inventory-file id)))
    
     vino-availability-add-fn
     (lambda (id amount source date)
       (inventory-add wine-inventory-file id amount source date))
    
     vino-availability-sub-fn
     (lambda (id amount action date)
       (inventory-sub wine-inventory-file id amount action date))
    
     vino-sources-fn
     (lambda (_)
       (inventory-sources wine-inventory-file)))

Optional variables (with their default values):

    (setq
     vino-carbonation-types '(still
                              sparkling)
    
     vino-colour-types '(red
                         white
                         rose)
    
     vino-sweetness-levels (list 'still '(dry
                                          semi-dry
                                          semi-sweet
                                          sweet)
                                 'sparkling '(brut-nature
                                              extra-brut
                                              brut
                                              extra-dry
                                              dry
                                              demi-sec
                                              doux))
    
     vino-rating-extra-meta (list
                             (list
                              :name "location"
                              :read-fn (lambda ()
                                         (vulpea-select "Location"))
                              :mode 'single
                              :type 'note)
                             (list
                              :name "convive"
                              :read-fn (lambda ()
                                         (vulpea-select
                                          "Convive"
                                          :filter-fn (lambda (note)
                                                       (seq-contains-p
                                                        (vulpea-note-tags note)
                                                        "people"))))
                              :mode 'multiple
                              :type 'note))
    
     vino-rating-template (list :file-name "wine/rating/${id}.org")
    
     vino-entry-template (list :file-name "wine/cellar/${id}.org")
    
     vino-grape-template (list :file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}.org")
    
     vino-producer-template (list :file-name "wine/producer/%<%Y%m%d%H%M%S>-${slug}.org")
    
     vino-region-template (list :file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}.org")
    
     vino-appellation-template (list :file-name "wine/appellation/%<%Y%m%d%H%M%S>-${slug}.org"))


<a id="org7d50894"></a>

## Interactive functions

-   `vino-entry-create` - create a new `vino-entry` according to
    `vino-entry-template`. It interactively reads carbonation type, colour type,
    sweetness level, producer, name, vintage, appellation or region, grapes,
    alcohol level, sugar, resources and price. Producer, appellation, region and
    grapes are linked using `org-roam`.
-   `vino-entry-find-file` - select and visit `vino-entry`.
-   `vino-entry-update` - update visiting (or selected) `vino-entry`. It refreshes
    `rating` and `availability` (using `vino-availability-fn`). You rarely need to
    use this function, unless availability or rating is modified manually.
-   `vino-entry-update-title` - update visiting (or selected) `vino-entry` title.
    It also changes the title of all linked `ratings`. You only need this function
    if you modify a producer name, wine entry name or vintage manually and want to
    update everything. Might take a while, depending on amount of linked
    `ratings`.
-   `vino-entry-set-grapes` - set grapes of visiting (or selected) `vino-entry` by
    replacing existing.
-   `vino-entry-set-region` - set region (or appellation) of visiting (or
    selected) `vino-entry` by replacing existing.
-   `vino-entry-acquire` - acquire visiting (or selected) `vino-entry`. Reads a
    source, amount, price and date, and calls `vino-availability-add-fn`.
-   `vino-entry-consume` - consume visiting (or selected) `vino-entry`. Reads a
    action, amount and date, and calls `vino-availability-sub-fn`. For convenience
    also asks you to rate entry if the action is `consume`.
-   `vino-entry-rate` - rate a visiting (or selected) `vino-entry`. Reads a date,
    props defined by `vino-rating-props`, creates a new rating note according to
    `vino-rating-template` and creates a link between wine and rating.
-   `vino-grape-create` - create a new `grape` note according to
    `vino-grape-template`.
-   `vino-grape-find-file` - select and visit `grape` note.
-   `vino-producer-create` - create a new `producer` note according to
    `vino-producer-template`.
-   `vino-producer-find-file` - select and visit `producer` note.
-   `vino-region-create` - create a new `region` note according to
    `vino-region-template`.
-   `vino-appellation-create` - create a new `appellation` note according to
    `vino-appellation-template`.
-   `vino-region-find-file` - select and visit `region` or `appellation` note.
-   `vino-db-sync` - build `vino` database cache.


<a id="org6f124d9"></a>

# Notes structure

`vino` assumes the following structure of your `org-roam-directory`:

    .
    └── wine
        ├── appellation
        │   ├── cerasuolo_di_vittoria_docg.org
        │   ├── etna_doc.org
        │   ├── igp_terre_siciliane.org
        │   └── ...
        ├── cellar
        │   ├── 2c012cee-878b-4199-9d3b-782d671bd198.org
        │   ├── 4faf700f-c8b9-403d-977c-8dee9e577514.org
        │   ├── b20373db-43d3-4f2c-992c-6c6b5a4f3960.org
        │   ├── c9937e3e-c83d-4d8d-a612-6110e6706252.org
        │   └── ...
        ├── grape
        │   ├── frappato.org
        │   ├── nerello_mascalese.org
        │   ├── nero_d_avola.org
        │   └── ...
        ├── producer
        │   ├── arianna_occhipinti.org
        │   ├── pyramid_valley.org
        │   └── ...
        ├── rating
        │   ├── be7777a9-7993-44cf-be9e-0ae65297a35d.org
        │   ├── bbc0c0f6-6f85-41a8-a386-f2017ceeaeb3.org
        │   ├── 727d03f3-828a-4957-aaa9-a19fd0438a15.org
        │   ├── d9e29c8e-06af-41d3-a573-72942cea64da.org
        │   ├── a5022e95-4584-43bd-ac55-599a942a6933.org
        │   └── ...
        └── region
            ├── central_otago.org
            ├── gisborne.org
            ├── kumeu.org
            └── ...

It's totally fine to have other notes in your `org-roam-directory` and even in
`wine` folder. Storing `vino` files in dedicated directories is not mandatory,
it just better organises notes.


<a id="org94b6b05"></a>

## Appellation/region

Each file represents either an appellation (like Cerasuolo di Vittoria DOCG or
Morgon AOC) or a wine region (like Central Otago in New Zealand or Codru in
Moldova). There are no restrictions on these files, except for presence of
`appellation` or `region` tag in addition to `wine` tag. See **Tags** section for more
information.

    $ cat wine/region/20201214120801-codru.org
    
    :PROPERTIES:
    :ID:                     b5758d14-61a2-4255-a47d-3cff3b58b321
    :END:
    #+title: Codru
    #+filetags: wine region
    
    - country :: [[id:6ce0bd2d-9018-4c5f-b896-639a85a6e7a4][Moldova]]
    
    Codru wine region is located in the central area of [[id:6ce0bd2d-9018-4c5f-b896-639a85a6e7a4][Moldova]]. More than
    60% of vineyards are located in this region.
    
    Two biggest cellars in the world ([[id:2374143f-5b7e-46ae-9ffc-649f529aaf70][Mileștii Mici]] and [[id:849a36b0-b24b-49e6-9e5d-19fc7ee13a78][Cricova]]) are
    located here.


<a id="org6db2c10"></a>

## Producer

Each file represents a producer (like Occhipinti or Vino di Anna). There are no
restrictions on these files, except for presence of `producer` tag in addition
to `wine` tag. See **Tags** section for more information.

    $ cat wine/producer/20200511140611-arianna_occhipinti.org
    
    :PROPERTIES:
    :ID:                     8f62b3bd-2a36-4227-a0d3-4107cd8dac19
    :END:
    #+title: Arianna Occhipinti
    #+filetahs: wine producer @AriannaOcchipinti
    
    - resources :: [[https://www.bowlerwine.com/producer/occhipinti][bowlerwine.com]]
    
    Arianna Occhipinti is a winemaker from [[id:3717adb1-4815-4ba6-9730-a884554214c9][Vittoria]] who founded her own winery in
    2004, bottled her first commercial vintage in 2006 and today works exclusively
    with estate fruit. Her 25 hectares feature only autochthonous varieties - 50%
    [[id:b968250e-2035-4b18-bd9f-fce99d5f9915][Frappato]], 35% [[id:c9731b65-61f8-4007-9dbf-d54056f55cc4][Nero d'Avola]] and 15% white varieties [[id:63532852-c67a-4b8d-ac42-1ae9be28610e][Albanello]] and [[id:ab59e210-e7ed-4362-832c-4c4daa2b9e05][Zibibbo]]. Almost
    all vines are young as she planted them, most of them a guyot-trained. But she
    also added to her holdings 60-years-old albarello-trained vines which she
    initially rented.
    
    ...


<a id="orgaf95b47"></a>

## Grape

Each file represents a producer (like Occhipinti or Vino di Anna). There are no
restrictions on these files, except for presence of `producer` tag in addition
to `wine` tag. See **Tags** section for more information.

    $ cat wine/grape/20200406154953-nerello_mascalese.org
    
    :PROPERTIES:
    :ID:                     9c1a5bec-9390-429e-bea9-4f1cce05f79c
    :END:
    #+title: Nerello Mascalese
    #+filetags: wine grape
    
    - resources :: [[https://winefolly.com/grapes/nerello-mascalese/][Winefolly]]
    - resources :: [[https://italianwinecentral.com/variety/nerello-mascalese/][italianwinecentral.com]]
    
    A rare red Sicilian grape producing fine light to medium-bodied red wines
    reminiscent of Pinot Noir. The best examples are found growing on the volcanic
    soils of Mount Etna.
    
    Primary flavours:
    
    - Dried [[id:7a945d62-b5f0-4542-bb1a-f4c8f9dd736b][Cherry]]
    - Orange [[id:8403a37b-be67-4efc-92f1-377aea0c8c50][Zest]]
    - Dried [[id:83a86596-437f-4931-a147-af1bd7734d28][Thyme]]
    - [[id:76cef2c9-0fc7-4802-8873-1c78a6be21da][Allspice]]
    - Crushed [[id:3b843816-3c5b-4758-89f6-804596087881][Gravel]]
    
    Taste profile:
    
    - sweetness: bone-dry
    - body: medium-light
    - tannins: medium
    - acidity: medium-high
    - alcohol: 11.5-13.5% ABV
    
    Handling
    
    - serve: 12-15°C
    - glass type: [[id:a88ce31d-bfb0-4343-9359-c4a366ad6a6b][Aroma Collector Glass]]
    - decant: 30 minutes
    - cellar: 10+ years


<a id="orgdd57a7e"></a>

## Vino entry

Each file represents a wine, specified by producer, name and vintage. Obviously,
you don't need to create separate files for two bottles of La Stoppa Ageno 2015,
but you definitely need separate note from La Stoppa Ageno 2017 (vintage is
different).

It's best if you create a vino entry using `vino-entry-create` interactive
function. It reads all required information, creates new file (uses `ID` as file
name), fills it will provided information and links producer, grapes,
appellation and region.

Vino entry is defined as a `cl-struct`:

    (cl-defstruct vino-entry
      carbonation
      colour
      sweetness
      producer
      name
      vintage
      appellation
      region
      grapes
      alcohol
      sugar
      resources
      price
      acquired
      consumed
      rating
      ratings)

Most of the fields are mandatory, except for:

-   `vintage` - unless specified, printed as `NV` string;
-   `sugar` - unless specified, printed as `NA` string;
-   `rating` - unless `ratings` list is non-nil, printed as `NA` string;
-   `ratings` - unless empty, omitted from the file.

Title if the file is set automatically upon creation and can be updated using
`vino-entry-update-title` if you modify something manually. This also updates
the title of linked rating files.

Availability is modified using `vino-entry-acquire` and `vino-entry-consume`. In
case you edited availability manually outside, use `vino-entry-update` to sync
it.

Rating is updated automatically upon using `vino-entry-rate` and can be updated
using `vino-entry-update` if you modify any rating note manually.

Vino entry files require the presence of `cellar` tag in addition to `wine` tag.
See **Tags** section for more information.

    $ cat wine/cellar/c9937e3e-c83d-4d8d-a612-6110e6706252.org
    
    :PROPERTIES:
    :ID:                     c9937e3e-c83d-4d8d-a612-6110e6706252
    :END:
    #+title: Arianna Occhipinti Bombolieri BB 2017
    #+filetags: wine cellar
    
    - carbonation :: still
    - colour :: red
    - sweetness :: dry
    - producer :: [[id:8f62b3bd-2a36-4227-a0d3-4107cd8dac19][Arianna Occhipinti]]
    - name :: Bombolieri BB
    - vintage :: 2017
    - appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
    - grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
    - alcohol :: 13
    - sugar :: 1
    - price :: 50.00 EUR
    - acquired :: 2
    - consumed :: 1
    - available :: 1
    - resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
    - rating :: NA
    
    #+begin_quote
    Il Frappato stems from a dream which I had when I was a girl to make a wine that
    knows the land that I work, the air I breath, and my own thoughts. It is bitter,
    bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
    that most resembles me, brave, original and rebellious. But not only. It has
    peasant origins, for this it loves its roots and the past that it brings in;
    but, at the same time, it is able to fight to improve itself. It knows
    refinement without forgetting itself.
    
    Arianna Occhipinti
    #+end_quote


<a id="org1d40bf1"></a>

## Vino rating

Each file represents a rating or a tasting note, specified by vino entry and
tasting date. You should create a new rating using `vino-entry-rate`. It reads
rating values according to `vino-rating-props`, creates a file (with `ID` as
file name) and fills it will provided information. Then it links newly created
rating from vino entry and updates the latter.

Rating files require the presence of `rating` tag in addition to `wine` tag. See
**Tags** section for more information.

    $ cat wine/rating/f1ecb856-c009-4a65-a8d0-8191a9de66dd.org
    
    :PROPERTIES:
    :ID:                     f1ecb856-c009-4a65-a8d0-8191a9de66dd
    :END:
    #+title: Arianna Occhipinti Bombolieri BB 2017 - 2021-01-15
    #+filetags: wine rating
    
    - wine :: [[id:c9937e3e-c83d-4d8d-a612-6110e6706252][Arianna Occhipinti Bombolieri BB 2017]]
    - date :: 2021-01-15
    - version :: 1
    - score :: 14
    - score_max :: 20
    - total :: 7.0


<a id="org928c38d"></a>

## Tags

Each vino file must contain a `wine` tag in addition to type tag (`appellation`,
`region`, `producer`, `grape`, `cellar` or `rating`), meaning that each file
must contain a respective `#+filetags` property:

    :PROPERTIES:
    :ID:                     1f4e920e-bfd4-4624-8445-fa8480962c17
    :END:
    #+title: La Stoppa Ageno 2015
    #+filetags: wine cellar
    
    ...

Files are tagged automatically by `vino` when entities are being created. You
can not change that. But since tags in `#+filetags` property are subject to
inheritance, you should add `vino` tags to `org-tags-exclude-from-inheritance`
(which is done for you in `vino-setup`) or disable inheritance completely by
setting `org-use-tag-inheritance` to nil.


<a id="orgc6feea6"></a>

# Database

`vino` provides a dedicated database (just like `org-roam`) that can be used for
fast querying of information. Right now there are no interactive uses for this
database, they are planned in [v.0.3](https://github.com/d12frosted/vino/milestone/3) and [v0.4](https://github.com/d12frosted/vino/milestone/4). But you already can build
something on your own.

Available tables (see `vino-db--schemata` for full specification):

-   `cellar` - contains `vino-entry` (see **Vino entry**) with some technical metadata
    (`id`, `file` and `hash`);
-   `ratings` - contains `vino-rating` (see **Vino rating**) with some technical
    metadata (`id`, `file` and `hash`).

In order to build database you need to manually call `vino-db-sync` (either from
your `init.el` file after `vino-setup` or interactively).

If you properly installed `vino` (e.g. called `vino-setup`), then every time you
modify and save a file, database will be automatically updated via
`vino-db-update-file` function.

In order to query information from database you might use `vino-db-query`:

    ;; query top rated wines with rating >= 9
    (vino-db-query
     [:select [producer name vintage rating]
      :from cellar
      :where (>= rating 9)
      :order-by rating :desc])

Right now only low level `vino-db-query` is available, more functions will
become available in [v0.4](https://github.com/d12frosted/vino/milestone/4).


<a id="org4bae89b"></a>

# How to &#x2026;


<a id="org997a129"></a>

## Configure templates

All the notes created by `vino` are created using `vulpea-create` function
according to the configurable templates:

-   `vino-grape-template`
-   `vino-producer-template`
-   `vino-region-template`
-   `vino-appellation-template`
-   `vino-entry-template`
-   `vino-rating-template`

Each template is a property list accepting following values:

-   `:file-name` (mandatory) - file name relative to `org-roam-directory`;
-   `:head` (optional) - extra header of the created note;
-   `:body` (optional) - body of the created note;
-   `:tags` (optional) - extra tags for the created note;
-   `:properties` (optional) - extra properties to put into `PROPERTIES` block;
-   `:context` (optional) - extra variables for `:file-name`, `:head`, `:body`
    templates.

The template is transformed into `vulpea-create` call by:

-   providing title (usually read interactively);
-   providing `file-name` from template;
-   providing tags according to entity being created;
-   providing `head`, `body`, `properties` and `context` from template;
-   setting `:unnarrowed` and `:immediate-finish` both to `t`.


<a id="org726fda1"></a>

## Configure rating system

Rating is configured by `vino-rating-prop`. My experience shows that rating
system evolves over time. You start with something simple (like a capped
number), then little by little you start to make your rating system more
complex, until one day it's too complex and you return to something simpler :D

So `vino-rating-prop` is a list of all your rating systems, starting with the
first version up to your current. This variable has the following format:

    '((1 . PROPS)
      (2 . PROPS)
      (3 . PROPS)
      ...)

And `PROPS` defines a specific version of rating system:

    (("PROP_1" . PROP)
     ("PROP_2" . PROP)
     ("PROP_3" . PROP)
     ...)

Each `PROP` can be of one of the following types:

-   `number` - then the property value is a number inclusively between `0` and
    `PROP`, user is prompted for a number using `read-number` during `vino-entry-rate`;
-   `list` - then the property value is a number inclusively between `0` and the
    length of `PROP`, user is prompted to select one element from the list `car`'s
    using `completing-read` during `vino-entry-rate` and the `cdr` of selected
    element is used as value;
-   `function` - then the property value is a number between `0` and `cdr` of
    `PROP` result, function is called with without arguments during
    `vino-entry-rate` and `car` of the result is used as value.

Final score is calculated as sum of the values divided by sum of max values and
multiplied by 10. So the final rating is a floating number from `0` to `10`.

Here are several examples to illustrate.

1.  Simple rating system that allows user to assign a single number from `0` to
    `3` which is stored as `SCORE`.
    
        (setq vino-rating-props
              '((1 . (("SCORE" . 3)))))

2.  Another simple rating system that uses multiple properties.
    
        (setq vino-rating-props
              '((2 . (("AROMA_QUALITY" . 3)
                      ("AROMA_INTENSITY" . 2)
                      ("AROMA_COMPLEXITY" . 3)
                      ("BALANCE" . 3)
                      ("FLAVOURS" . 2)
                      ("AFTERTASTE" . 3)
                      ("GENERAL" . 4)))))

3.  A complex use cases that uses a function for `AROMA_QUALITY` (so default
    value is 3, but if wine has any taints, the value is decreased) and lists for
    everything else.
    
        (setq vino-rating-props
              '((3 . (("AROMA_QUALITY" .
                       (lambda ()
                         (let* ((total 3)
                                (res total)
                                (ans t)
                                (quit-on "no taints")
                                (opts (list
                                       quit-on
                                       "aggressive ethanol"
                                       "massive brett attack"
                                       "VA, especially nail polish removal")))
                           (while ans
                             (setq ans (completing-read "Any taints? " opts))
                             (setq opts (delete ans opts))
                             (if (string-equal ans "no taints")
                                 (setq ans nil)
                               (setq res (max 0 (- res 1))))
                             (when (equal res 0)
                               (setq ans nil)))
                           (cons res total))))
        
                      ("AROMA_INTENSITY" .
                       (("aroma can be perceived without putting nose into glass" . 2)
                        ("aroma can be perceived only by putting nose into glass" . 1)
                        ("closed, you need to put a lot of effort to get the aroma" . 0)))
        
                      ("AROMA_RICHNESS" .
                       (("more than 3 different notes" . 3)
                        ("only 3 notes" . 2)
                        ("only 2 notes" . 1)
                        ("only 1 note" . 0)))
        
                      ("AROMA_COMPLEXITY" .
                       (("sophisticated, multilayered" . 1)
                        ("simple" . 0)))
        
                      ("BALANCE" .
                       (("perfectly balanced, everything is in its place" . 3)
                        ("well balanced, might be a small issue" . 2)
                        ("average, either one bigger issue or two small" . 1)
                        ("unbalanced, everything else" . 0)))
        
                      ("FLAVOURS" .
                       (("multiple flavours" . 1)
                        ("only one flavour" . 0)))
        
                      ("EVOLUTION" .
                       (("taste and flavours evolve over time in mouth" . 1)
                        ("plain, straightforward" . 0)))
        
                      ("AFTERTASTE" .
                       (("long, lasting more than 30 seconds" . 2)
                        ("average, lasting more than 10 seconds" . 1)
                        ("short" . 0)))
        
                      ("GENERAL" .
                       (("life changing" . 4)
                        ("great wine, I will definitely look into tasting it once more" . 3)
                        ("good wine, will drink it again with pleasure if situation arises" . 2)
                        ("average wine, only with parents" . 1)
                        ("bad wine, only for enemies" . 0)))))))


<a id="org6d02485"></a>

## Store images and other attachments

`vino` operates with `org-mode` files, meaning that you can use `org-attach` to
store images as well as other attachments. Refer to [Org mode documentation](https://orgmode.org/manual/Attachments.html#Attachments) for
more information.


<a id="orgdcfaa21"></a>

## Query additional metadata when creating a new wine entry

See [vino#65](https://github.com/d12frosted/vino/issues/65).


<a id="org46d9b0b"></a>

## Query additional metadata when rating a wine entry

See [vino#64](https://github.com/d12frosted/vino/issues/64).


<a id="org2667c8a"></a>

# Coding

`vino` is developed using [eldev](https://github.com/doublep/eldev/). If you are using `flycheck`, it is advised to
also use [flycheck-eldev](https://github.com/flycheck/flycheck-eldev), as it makes dependencies and project files available
thus mitigating false negative results from default Emacs Lisp checker.


<a id="org4bf21c4"></a>

# Building and testing

`vino` tests are written using [buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) testing framework. And [eldev](https://github.com/doublep/eldev/) is used to
run them both locally and on CI. In order to run the tests locally, first
[install](https://github.com/doublep/eldev/#installation) `eldev` and then run:

    $ make test

Please note, that the linter is used in this project, so you might want to run
it as well:

    $ make lint


<a id="org326bb36"></a>

# FAQ


<a id="orgbeae03e"></a>

## Why not generalise?

My experience shows that some parts of the code base can be shared for tracking
other things, like tea (I have a decent collection of tea, that I also track and
rate) and books. And I am sure there are many more uses cases.

But since most of the time I write about wine, I want to focus solely on this
topic and avoid making perfect an enemy of good.

That being said, please contact me if you wish to use it for other things, I
would love to hear your use case and help you with building solution for you.


<a id="org215c4f7"></a>

## Why not Cellar Tracker, Vivino, etc.?

Frankly speaking, I don't trust them to be my source of truth. In my sense both
services have the following drawbacks:

-   Data is not owned by you.
-   No API to get *your* information.
-   There is no way to modify invalid data.
-   Requires internet connection.
-   Not helpful for learning - every piece of information is already there.
-   Hard limit on amount of information you can put there.
-   Not extensible.

`vino` is about learning about wine, owning your data and extending your tools.
With the power of `org-roam` you can do everything :)

That being said, I still use Vivino for:

-   Reading tasting notes of peoples whose opinion I respect. This also helps me
    to find new interesting bottles available in my location.
-   Sharing some of my notes. This stimulates me to work on short and concise
    tasting notes.

So you can use both!


<a id="org0caadc1"></a>

# Acknowledgements

[Logo](images/logo.png) was created by [Iryna Rutylo](https://www.behance.net/irynarutylo).

