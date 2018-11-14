(ns sample.core
(:gen-class))

(def connectionString "Server=localhost\\SQLEXPRESS;Database=GSB_CPQ_DEV;Persist Security Info=True;Integrated Security=SSPI")
(clojure.core/assembly-load-from "System.Data.dll")

(defn cast-null [colec]
  (map (fn [item] (if (= System.DBNull (type item)) nil item)) colec))

(defn select [query tablename]
  (let [con (new System.Data.SqlClient.SqlConnection connectionString)
        da (new System.Data.SqlClient.SqlDataAdapter query con)
        ds (new System.Data.DataSet)]
        (do
          (.Open con)
          (.Fill da ds tablename)
          (let [data (.Tables ds)
                table (first data)
                table-keys (map (fn [column] (keyword (clojure.string/lower-case (.ColumnName column) ))) (.Columns table))
                table-values (map (fn [row] (.ItemArray row)) (.Rows table))]
            (do
              (.Close con)
              (map (fn [row] (zipmap table-keys (cast-null row ))) table-values))))))

(defn sql-command [command]
  (let [con (new System.Data.SqlClient.SqlConnection connectionString)
        cmd (new System.Data.SqlClient.SqlCommand)]
    (do
      (.Open con)
      (set! (. cmd Connection) con)
      (set! (. cmd CommandText) command)
      (.ExecuteNonQuery cmd)
      (.Close con))))

(def data (sample.core/select "select top 2 * from quotes" "quotes"))
(sample.core/sql-command "update quotes set istierstandard = 1 where id =69")
