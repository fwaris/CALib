module TraceCharts
open FSharp.Charting
open System.Windows.Forms.DataVisualization
open System.Windows.Forms

let applyBgHost image (chh:ChartTypes.ChartControl) = 
    let ch = chh.Controls.[0] :?> System.Windows.Forms.DataVisualization.Charting.Chart
    ch.ChartAreas.[0].BackImage <- image
    ch.ChartAreas.[0].BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled

let applyBg imageOpt (c:FSharp.Charting.ChartTypes.GenericChart) = 
    match imageOpt with
    | Some image ->
        c.ApplyToChart (fun c -> 
            let a = c.ChartAreas.[0]
            a.BackImage <- image
            a.BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled)
    | None -> c

let chGrid = ChartTypes.Grid(Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true, Interval=0.2, Format="{0:F1}")

let round' x = System.Math.Round((x:float),2)

let chPoints bg title obs =
    let ch =
        LiveChart.Point(obs , Title=title) 
        |> Chart.WithTitle(Color=System.Drawing.Color.Gold)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
        |> applyBg bg
    ch,bg

let chCounts obs =
    let ch =
        LiveChart.Column(obs, Title="Live KS Counts") 
        |> Chart.WithStyling(Color=System.Drawing.Color.Tomato)
    ch,(None:string option)

let chDisp title obs =  LiveChart.FastLineIncremental(obs,Title=title),(None:string option)

let private containerize (ch,bg) = 
    let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
    match bg with
    | Some image -> applyBgHost image chh
    | None       -> ()
    chh

let container chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 300
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 3
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.RowCount <- 2
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    chlist |> List.map containerize |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
