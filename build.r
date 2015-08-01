meta = c("Slovakia", "Národná rada")
mode = "fruchtermanreingold"

legislatures = c(
  "1" = "1994-1998",
  "2" = "1998-2002",
  "3" = "2002-2006",
  "4" = "2006-2010",
  "5" = "2010-2012",
  "6" = "2012-2016"
)

for(ii in unique(a$legislature)) {

  cat("\nLegislature", ii, "years", legislatures[ ii ])
  data = subset(a, legislature == ii & n_au > 1)

  sp = subset(s, legislature == ii) %>% data.frame
  stopifnot(!duplicated(sp$uid))

  cat(":", nrow(data), "cosponsored documents, ")

  #
  # directed edge list
  #

  edges = lapply(data$authors, function(d) {

    w = unlist(strsplit(d, ", "))

    d = expand.grid(i = sp$uid[ sp$uid %in% w  ],
                    j = sp$uid[ sp$uid == w[1] ], stringsAsFactors = FALSE)

    return(data.frame(d, w = length(w) - 1)) # number of cosponsors

  }) %>% bind_rows

  #
  # edge weights
  #

  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = subset(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # sanity check
  stopifnot(edges$gsw <= 1)

  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)

  cat(nrow(edges), "edges, ")

  #
  # directed network
  #

  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$date, 1, 4))),
                                        collapse = " to "))

  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(a, legislature == ii)$n_au)

  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$uid
  n %v% "url" = paste0(sp[ network.vertex.names(n), "id" ],
                       "&CisObdobia=", ii) %>% as.character
  n %v% "sex" = as.character(sp[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(sp[ network.vertex.names(n), "born" ], 1, 4))
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  n %v% "constituency" = sp[ network.vertex.names(n), "county" ]
  n %v% "nyears" = as.numeric(sp[ network.vertex.names(n), "nyears" ])
  n %v% "photo" = as.character(sp[ network.vertex.names(n), "photo" ]) %>%
    gsub("http://www.nrsr.sk/web/dynamic/PoslanecPhoto.aspx\\?PoslanecID=|&ImageWidth=140",
         "", .)

  # unweighted degree
  n %v% "degree" = degree(n)
  q = n %v% "degree"
  q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))

  set.edge.attribute(n, "source", as.character(edges[, 1]))
  set.edge.attribute(n, "target", as.character(edges[, 2]))

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  #
  # network plot
  #

  if(plot) {

    save_plot(n, file = paste0("plots/net_sk", legislatures[ ii ]),
               i = colors[ sp[ n %e% "source", "party" ] ],
               j = colors[ sp[ n %e% "target", "party" ] ],
               q, colors, order)

  }

  #
  # save objects
  #

  # replace uids with names
  network.vertex.names(n) = sp[ network.vertex.names(n), "name" ]

  set.edge.attribute(n, "source", sp[ n %e% "source", "name" ])
  set.edge.attribute(n, "target", sp[ n %e% "target", "name" ])

  edges$i = sp[ edges$i, "name" ]
  edges$j = sp[ edges$j, "name" ]

  # check the names are unique
  u = network.vertex.names(n)[ duplicated(network.vertex.names(n)) ]
  stopifnot(!length(u))

  assign(paste0("net_sk", substr(legislatures[ ii ], 1, 4)), n)
  assign(paste0("edges_sk", substr(legislatures[ ii ], 1, 4)), edges)
  assign(paste0("bills_sk", substr(legislatures[ ii ], 1, 4)), data)

  #
  # export gexf
  #

  if(gexf) {

    n %v% "url" = gsub("&", "&amp;", n %v% "url")
    save_gexf(paste0("net_sk", legislatures[ ii ]), n, meta, mode, colors,
             extra = "constituency")

  }

}

if(gexf)
  zip("net_sk.zip", dir(pattern = "^net_sk\\d{4}-\\d{4}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_sk\\d{4}$"),
     file = "data/net_sk.rda")
