package cs305.minesweeper.winmine;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.Raster;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

import cs305.minesweeper.MinesweeperBoard;
import cs305.minesweeper.MinesweeperTile;

/**
 * 
 * @author Michael Sergio
 *
 */
public class WinMineBoard implements MinesweeperBoard {

	private static final int RESET_BUTTON = KeyEvent.VK_F2;
	private static final Rectangle SCREEN_SIZE = new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
	private static final Color BOARD_BOUNDRY = new Color(128,128,128);
	private static final Color EMPTY_COLOR = new Color(192,192,192);

	private GameState state = GameState.Waiting;
	
	Dimension amountOfTiles;
	Dimension tileSize;
	
	WinMineTile[][] theGame;
	
	
	Dimension offsetCorner; // corner from which all operations will offset
	int length;
	int width;
	Robot r;
	private int actionsTaken;

	public WinMineBoard() {
		try {
			r = new Robot();
		} catch (AWTException e) {
			e.printStackTrace();
		}
		
	}
	
	private void initializeBoard() {
		try {
			detectBoardCornerPoint();
			detectWidth();
			detectNumberOfTiles();
			figureOutTileSize();
			
			state = GameState.Playing;
			theGame = new WinMineTile[amountOfTiles.width][amountOfTiles.height];
			
			blankOutBoard();
			
		} catch (AWTException e) {
			e.printStackTrace();
		}
	}

	private void blankOutBoard() {
		for (int x  = 0; x < theGame.length; x++)
			for (int y = 0; y < theGame[x].length ; y++)
				theGame[x][y] = new WinMineTile(x,y, this);
	}

	private void figureOutTileSize() {
		tileSize = new Dimension(width  / amountOfTiles.width, length / amountOfTiles.height);
		
	}

	private void detectBoardCornerPoint() throws AWTException {
		// This nasty thing determines the longest vertical line of a color.
		// This color determines the boundary color of Minesweeper area.
		// On Windows XP version of Minesweeper, the color is rgb:128,128,128 by default

		Raster raster = r.createScreenCapture(SCREEN_SIZE).getRaster();

		int capWidth = raster.getWidth();
		int capHeight = raster.getHeight();

		System.out.println(capWidth + "," + capHeight);

		Dimension bestHeightStartingPoint = null;
		int bestWidth = 0;

		Dimension tempHeightStartingPoint = null;
		int tempWidth = 0;

		// Abandon every hope, ye who enter here - Inferno Canto III - Dante Alighieri
		for (int y = 0; y < capHeight; y++) {
			for (int x = 0; x < capWidth; x++) {
				if (r.getPixelColor(x, y).equals(BOARD_BOUNDRY)) {
					if (tempHeightStartingPoint == null) {
						tempHeightStartingPoint = new Dimension(x, y);
					}
					else {
						tempWidth++;
					}
					x--;
					y++;
				}
				else if (tempHeightStartingPoint != null) {
					if (tempWidth >= bestWidth){
						System.out.println(x + ", "+ y);
						bestWidth = tempWidth;
						bestHeightStartingPoint = tempHeightStartingPoint;//record the number if its the greatest
						tempHeightStartingPoint = null;
					}					
					
					//reset the counters
					tempWidth = 0;
					tempHeightStartingPoint = null;
				}
				
			}
		}			

		System.out.println("LENGTH: " + bestWidth);
		length = bestWidth;
		offsetCorner = bestHeightStartingPoint;
	}

	private void detectWidth() {
		int x = offsetCorner.width;
		int y = offsetCorner.height;
		
		while (r.getPixelColor(x, y).equals(BOARD_BOUNDRY)) {
			x++;
		}
		
		width = x - offsetCorner.width;
	}

	private void detectNumberOfTiles() {
		amountOfTiles = new Dimension(figureXCount() ,figureYCount());

	}	
	
	private int figureXCount() {
		// Gets the amount of tiles in the X axis
		//get to middle
		int middleLength = (length / 2) + offsetCorner.height;
		//detect the tiles empty color
		boolean onColor = false;
		
		int xCount = 0;

		for (int xPos = offsetCorner.width; xPos < offsetCorner.width + width; xPos++) {
		
			r.mouseMove(xPos, middleLength);
			
			
			if (r.getPixelColor(xPos, middleLength).equals(EMPTY_COLOR) && !onColor) {
				xCount++;
				onColor = true;
			}
			else if (!r.getPixelColor(xPos, middleLength).equals(EMPTY_COLOR) && onColor) {
				onColor = false;
			}
			
		}
		
		return xCount;
		
	}
	
	private int figureYCount() {
		//Gets the amount of tiles in the Y Axis
		//get to middle
		int middleWidth = (width / 2) + offsetCorner.width;
		//detect the tiles empty color
		boolean onColor = false;
		
		int yCount = 0;
		
		for (int yPos = offsetCorner.height; yPos < offsetCorner.height+ length; yPos++) {
			r.mouseMove(middleWidth, yPos);
			
			
			if (r.getPixelColor(middleWidth, yPos).equals(EMPTY_COLOR) && !onColor) {
				yCount++;
				onColor = true;
			}
			else if (!r.getPixelColor(middleWidth, yPos).equals(EMPTY_COLOR) && onColor)
				onColor = false;
		}
		
		return yCount;
		
	}
	

	private void gainFocus() {
		r.mouseMove(offsetCorner.width, offsetCorner.height);
		r.mousePress(InputEvent.BUTTON1_MASK);
		r.delay(100);
		r.mouseRelease(InputEvent.BUTTON1_MASK);
	}

	public String boardInfo() {
	
		StringBuilder sb = new StringBuilder();
		sb.append(offsetCorner.toString() + '\n');
		sb.append("Length: " + length + '\n');
		sb.append("Width: " + width + '\n');
		sb.append("Number of Tiles: " + amountOfTiles.width + "," + amountOfTiles.height + '\n');
		sb.append("Tile Area: " + tileSize.width + " x " + tileSize.height + '\n' );
		return sb.toString();
	}
	
	@Override
	public String toString() {

		StringBuilder sb = new StringBuilder();
		sb.append("vvvvvvvvvv\n");
		
			for (int y = 0; y < theGame.length ; y++) {
				for (int x  = 0; x < theGame[y].length; x++) {			
				sb.append(" " + theGame[y][x].prettyString() + " ");

			}
			sb.append('\n');
		}

		sb.append("^^^^^^^^^^");
		return sb.toString();

		
	
	}
	
	public static void main(String[] args) throws AWTException {
		WinMineBoard board = new WinMineBoard();
		board.displayBoard();

		for (MinesweeperTile t : board.getAllTiles()) 
		{
			board.flagTile(t);
			System.out.println(board.state);
			System.out.println(t.getValue());
			if (board.state == GameState.Lost)
				break;
			
		}
		
		board.resetBoard();
	}

	@Override
	public void clickTile(MinesweeperTile tile) {
		System.out.println(this);
		
		WinMineTile wTile = (WinMineTile) tile;
		Dimension centerPoint = wTile.getCenterPoint();
		

		// double clicking looks nice than gaining focus
		for (int i = 0; i < 2; i++) {
			r.mouseMove(centerPoint.width, centerPoint.height);
			r.mousePress(InputEvent.BUTTON1_MASK);
			r.delay(100);
			r.mouseRelease(InputEvent.BUTTON1_MASK);
		}

		actionsTaken++;
		// FIXME There's a new record case that can fuck up my screen capture
		// I wanted to take that case into account but was unable to reproduce it
		
		// Redetermine information
		for (int x  = 0; x < theGame.length; x++)
			for (int y = 0; y < theGame[x].length ; y++)
				theGame[x][y].determineInformation();
		
		//check for loss
		if (wTile.getValue() == MinesweeperTile.BOMB_VALUE)
			state = GameState.Lost;
	}

	@Override
	public void flagTile(MinesweeperTile tile) {
	
		// right click on board to gain focus if not present
		WinMineTile wTile = (WinMineTile) tile;
		gainFocus();
		
		Dimension centerPoint = wTile.getCenterPoint();
		r.mouseMove(centerPoint.width, centerPoint.height);
		r.mousePress(InputEvent.BUTTON3_MASK);
		r.delay(100);
		r.mouseRelease(InputEvent.BUTTON3_MASK);
		
		wTile.flagTile();
		actionsTaken++;
	
	}


	@Override
	public void clickTile(int x, int y) {
		clickTile(theGame[x][y]);
	}

	@Override
	public void flagTile(int x, int y) {
		flagTile(theGame[x][y]);
	}

	@Override
	public MinesweeperTile[] getAllTiles() {
		Queue<MinesweeperTile> tileList = new LinkedList<MinesweeperTile>(); 
		
		for (int x  = 0; x < theGame.length; x++)
			for (int y = 0; y < theGame[x].length ; y++)
				tileList.offer(theGame[x][y]);
		
		MinesweeperTile[] allTiles = new WinMineTile[tileList.size()];
		for (int i = 0; i < allTiles.length; i++) {
			allTiles[i] = tileList.poll();
		}
		
		return allTiles;
	}


	@Override
	public Dimension getBoardSize() {
		return amountOfTiles;
	}

	@Override
	public GameState getGameState() {
		if (getHiddenTiles().length == 0 && state != GameState.Lost) 
			state = GameState.Won;
		return state;
	}

	@Override
	public MinesweeperTile getTile(int x, int y) {
		return theGame[x][y];
	}

	@Override
	public MinesweeperTile[] getNumberTiles() {
		ArrayList<MinesweeperTile> tilesList = new ArrayList<MinesweeperTile>(amountOfTiles.height * amountOfTiles.width);
		
		for (int x  = 0; x < theGame.length; x++)
			for (int y = 0; y < theGame[x].length ; y++) {
				
				int tileValue = theGame[x][y].getValue();
				
				if (tileValue > 0 && tileValue < 9) // if a number	
					tilesList.add(theGame[x][y]);
			}
	
		
		MinesweeperTile[] tiles = new WinMineTile[tilesList.size()];
		int i = 0;
		for (MinesweeperTile t : tilesList)
			tiles[i++] = t;
		
		return tiles;
	}

	@Override
	public MinesweeperTile[]  getHiddenTiles() {
		ArrayList<MinesweeperTile> unknownList = new ArrayList<MinesweeperTile>(amountOfTiles.height * amountOfTiles.width);
		
		
		for (int x  = 0; x < theGame.length; x++)
			for (int y = 0; y < theGame[x].length ; y++) {
				
				if (theGame[x][y].getValue() == MinesweeperTile.UNKNOWN_VALUE)
					unknownList.add(theGame[x][y]);
			}
		
		
		MinesweeperTile[] tiles = new WinMineTile[unknownList.size()];
		int i = 0;
		for (MinesweeperTile t : unknownList)
			tiles[i++] = t;
		
		return tiles;
	}

	@Override
	public void displayBoard() {
		try {
			Runtime.getRuntime().exec("winmine");
			Thread.sleep(1000);
			initializeBoard();
			
			
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}


	@Override
	public void resetBoard() {
		gainFocus();
		r.keyPress(RESET_BUTTON);
		r.keyRelease(RESET_BUTTON);
		blankOutBoard();
		state = GameState.Playing;
	}
	

}
