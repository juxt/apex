;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.petstore.util)

;; TODO: Once we are happy with the design, this will be promoted to
;; an Apex module.
(defn create-reitit-route-map
  "Return a map of paths to Reitit routes. Routes contain a
  default :handler entry which can be overridden. Routes also contain
  the OpenAPI operation under the :apex/operation key."
  [openapi]
  (into {}
        (map (fn [[path path-item]]
               [path (into {}
                           (map (fn [[method operation]]
                                  [(keyword method)
                                   {:handler
                                    (fn [req respond raise]
                                      (respond {:status 200
                                                :body (format "Coming soon! TODO: Mount a handler (and maybe some middleware) to %s and method %s (operationId is '%s')" path (.toUpperCase method) (get operation "operationId"))}))
                                    :apex/operation operation

                                    }]
                                  ) path-item))])
             (get openapi "paths"))))
