# trade flow
using DataFrames, CSV, Chain, LinearAlgebra, DataFramesMeta, Statistics

cd("/sfp/input")

cp_cent_rmPA = @chain CSV.read("cp.cent.rmPA.csv", DataFrame) begin
    @select(:cp_pixel, :index2010)
end

trade_full = @chain CSV.read("agr.imp.rmPA.pro.csv", DataFrame) begin
    @select(:exp_iso3 = :"exp.iso3", :imp_iso3 = :"imp.iso3", :SPAM_short_name = :"SPAM.short.name", :crop_area = :"crop.area", :cp_pixel)
    @rsubset(:crop_area > 0)
    leftjoin(_, cp_cent_rmPA, on=:cp_pixel)
end

# Very high CP
df_VH = @chain trade_full begin
    @rsubset(:index2010 > 0.9, :imp_iso3 != "ZZZ")
    @by([:exp_iso3, :imp_iso3, :SPAM_short_name], :crop_area = sum(skipmissing(:crop_area)))
end
CSV.write("tradeVH.csv", df_VH)

# High CP
df_H = @chain trade_full begin
    @rsubset((:index2010 > 0.75) & (:index2010 <= 0.9), :imp_iso3 != "ZZZ")
    @by([:exp_iso3, :imp_iso3, :SPAM_short_name], :crop_area = sum(skipmissing(:crop_area)))
end
CSV.write("tradeH.csv", df_H)

# Medium CP
df_M = @chain trade_full begin
    @rsubset((:index2010 > 0.5) & (:index2010 <= 0.75), :imp_iso3 != "ZZZ")
    @by([:exp_iso3, :imp_iso3, :SPAM_short_name], :crop_area = sum(skipmissing(:crop_area)))
end
CSV.write("tradeM.csv", df_M)

# Low CP
df_L = @chain trade_full begin
    @rsubset(:index2010 <= 0.5, :imp_iso3 != "ZZZ")
    @by([:exp_iso3, :imp_iso3, :SPAM_short_name], :crop_area = sum(skipmissing(:crop_area)))
end
CSV.write("tradeL.csv", df_L)

# Total
df_Total = @chain trade_full begin
    @rsubset(:imp_iso3 != "ZZZ")
    @by([:exp_iso3, :imp_iso3, :SPAM_short_name], :crop_area = sum(skipmissing(:crop_area)))
end
CSV.write("tradeTotal.csv", df_Total)