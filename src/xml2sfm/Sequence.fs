namespace xml2sfm

module Sequence =
    let append item sequence =
        seq { yield! sequence; yield item }

    let prepend item sequence =
        seq { yield item; yield! sequence }
